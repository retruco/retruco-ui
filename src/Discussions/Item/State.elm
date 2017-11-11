module Discussions.Item.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Constants exposing (discussionKeyIds)
import Decoders
import Discussions.Item.Types exposing (..)
import Discussions.NewSuggestion.State
import Http
import I18n
import Json.Decode
import Navigation
import Ports
import Requests
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language objectId =
    { authentication = authentication
    , data = initData
    , discussionPropertyIds = Nothing
    , httpError = Nothing
    , language = language
    , newSuggestionModel = Discussions.NewSuggestion.State.init authentication language objectId
    , objectId = objectId
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newSuggestionModel = Discussions.NewSuggestion.State.mergeModelData mergedData model.newSuggestionModel
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newSuggestionModel =
            Discussions.NewSuggestion.State.setContext
                authentication
                language
                model.newSuggestionModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewSuggestionMsg (Discussions.NewSuggestion.State.subscriptions model.newSuggestionModel)
        , Ports.propertyUpserted PropertyUpserted
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSuggestionMsg childMsg ->
            let
                ( updatedNewSuggestionModel, childCmd ) =
                    Discussions.NewSuggestion.State.update childMsg model.newSuggestionModel
            in
                ( { model | newSuggestionModel = updatedNewSuggestionModel }
                , Cmd.map translateNewSuggestionMsg childCmd
                )

        PropertyUpserted propertyJson ->
            case Json.Decode.decodeValue Decoders.graphqlPropertyDecoder propertyJson of
                Err message ->
                    let
                        _ =
                            Debug.log "PropertyUpserted Decode error:" message

                        _ =
                            Debug.log "PropertyUpserted JSON:" propertyJson
                    in
                        ( model, Cmd.none )

                Ok data ->
                    let
                        mergedModel =
                            mergeModelData data model

                        propertyId =
                            data.id
                    in
                        ( { mergedModel
                            | discussionPropertyIds =
                                case model.discussionPropertyIds of
                                    Just discussionPropertyIds ->
                                        if List.member propertyId <| Array.toList discussionPropertyIds then
                                            Just discussionPropertyIds
                                        else
                                            Just <| Array.append (Array.fromList [ propertyId ]) discussionPropertyIds

                                    Nothing ->
                                        Just <| Array.fromList [ propertyId ]
                          }
                        , Cmd.none
                        )

        Retrieve ->
            ( { model
                | discussionPropertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getProperties model.authentication model.showTrashed [ model.objectId ] discussionKeyIds []
                |> Http.send (ForSelf << Retrieved)
            )

        Retrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        Retrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                { mergedModel
                    | discussionPropertyIds = Just data.ids
                }
                    ! [ Ports.subscribeToPropertyUpserted [ model.objectId ] discussionKeyIds []
                      , Ports.setDocumentMetadata
                            { description = I18n.translate language I18n.PropertiesDescription
                            , imageUrl = Urls.appLogoFullUrl
                            , title = I18n.translate language I18n.Properties
                            }
                      ]

        SuggestionUpserted data ->
            ( model, Cmd.none )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve
        { model
            | showTrashed = Urls.queryToggle "trashed" location
        }
