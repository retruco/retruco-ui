module Properties.SameObjectAndKey.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Data exposing (initData, mergeData)
import Http
import I18n
import Navigation
import Ports
import Properties.SameObjectAndKey.Types exposing (..)
import Requests
import Types exposing (..)
import Urls
import Values.New.State


init : Maybe Authentication -> Bool -> I18n.Language -> String -> String -> Model
init authentication embed language objectId keyId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , httpError = Nothing
    , keyId = keyId
    , language = language
    , newValueModel = Values.New.State.init authentication embed language []
    , objectId = objectId
    , propertyIds = Nothing
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newValueModel = Values.New.State.mergeModelData mergedData model.newValueModel
        }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
        , newValueModel = Values.New.State.setContext authentication embed language model.newValueModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewValueMsg (Values.New.State.subscriptions model.newValueModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewValueMsg childMsg ->
            let
                ( updatedNewValueModel, childCmd ) =
                    Values.New.State.update childMsg model.newValueModel
            in
                ( { model | newValueModel = updatedNewValueModel }
                , Cmd.map translateNewValueMsg childCmd
                )

        Retrieve ->
            ( { model
                | httpError = Nothing
                , propertyIds = Nothing
              }
            , Requests.getProperties model.authentication False [ model.objectId ] [ model.keyId ] []
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
                    | propertyIds = Just data.ids
                  }
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.PropertiesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Properties
                    }
                )

        Upserted (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        Upserted (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | propertyIds =
                        case model.propertyIds of
                            Just propertyIds ->
                                if List.member data.id <| Array.toList propertyIds then
                                    Just propertyIds
                                else
                                    Just <| Array.append (Array.fromList [ data.id ]) propertyIds

                            Nothing ->
                                Just <| Array.fromList [ data.id ]
                  }
                , Cmd.none
                )

        ValueUpserted data ->
            ( mergeModelData data model
            , Requests.postProperty model.authentication model.objectId model.keyId data.id (Just 1)
                |> Http.send (ForSelf << Upserted)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve model
