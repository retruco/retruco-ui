module DebateProperties.SameObject.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import DebateProperties.New.State
import DebateProperties.SameObject.Types exposing (..)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language objectId =
    { authentication = authentication
    , data = initData
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , language = language
    , newDebatePropertyModel = DebateProperties.New.State.init authentication language objectId []
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


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newDebatePropertyModel =
            DebateProperties.New.State.setContext
                authentication
                language
                model.newDebatePropertyModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewArgumentMsg (DebateProperties.New.State.subscriptions model.newDebatePropertyModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewArgumentMsg childMsg ->
            let
                ( updatedNewDebatePropertyModel, childCmd ) =
                    DebateProperties.New.State.update childMsg model.newDebatePropertyModel
            in
                ( { model | newDebatePropertyModel = updatedNewDebatePropertyModel }
                , Cmd.map translateNewArgumentMsg childCmd
                )

        Retrieve ->
            ( { model
                | debatePropertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getObjectProperties model.authentication model.showTrashed model.objectId debateKeyIds []
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
                , -- TODO
                  Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.CardsDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Cards
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
                                if List.member data.id debatePropertyIds then
                                    Just debatePropertyIds
                                else
                                    Just (data.id :: debatePropertyIds)

                            Nothing ->
                                Just [ data.id ]
                  }
                , Cmd.none
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve
        { model
            | showTrashed = Urls.queryToggle "trashed" location
        }