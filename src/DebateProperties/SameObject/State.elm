module DebateProperties.SameObject.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import Data exposing (initData, mergeData)
import DebateProperties.New.State
import DebateProperties.SameObject.Types exposing (..)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , language = language
    , newDebatePropertyModel = DebateProperties.New.State.init authentication embed language objectId [ "TextField" ]
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
            , newDebatePropertyModel = DebateProperties.New.State.mergeModelData mergedData model.newDebatePropertyModel
        }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
        , newDebatePropertyModel =
            DebateProperties.New.State.setContext
                authentication
                embed
                language
                model.newDebatePropertyModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewDebatePropertyMsg (DebateProperties.New.State.subscriptions model.newDebatePropertyModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDebatePropertyMsg childMsg ->
            let
                ( updatedNewDebatePropertyModel, childCmd ) =
                    DebateProperties.New.State.update childMsg model.newDebatePropertyModel
            in
                ( { model | newDebatePropertyModel = updatedNewDebatePropertyModel }
                , Cmd.map translateNewDebatePropertyMsg childCmd
                )

        Retrieve ->
            ( { model
                | debatePropertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getProperties model.authentication model.showTrashed [ model.objectId ] debateKeyIds []
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
                ( { mergedModel
                    | debatePropertyIds = Just data.ids
                  }
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.PropertiesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Properties
                    }
                )

        Upserted data ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | debatePropertyIds =
                        case model.debatePropertyIds of
                            Just debatePropertyIds ->
                                if List.member data.id <| Array.toList debatePropertyIds then
                                    Just debatePropertyIds
                                else
                                    Just <| Array.append (Array.fromList [ data.id ]) debatePropertyIds

                            Nothing ->
                                Just <| Array.fromList [ data.id ]
                  }
                , Cmd.none
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve
        { model
            | showTrashed = Urls.queryToggle "trashed" location
        }
