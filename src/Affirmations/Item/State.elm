module Affirmations.Item.State exposing (..)

import Affirmations.Item.Routes exposing (..)
import Affirmations.Item.Types exposing (..)
import Arguments.New.State
import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import Http
import I18n
import Navigation
import Ports
import Process
import Requests
import Task
import Time
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { authentication = authentication
    , data = initData
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , id = id
    , language = language
    , newArgumentModel = Arguments.New.State.init authentication language id []
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
            , newArgumentModel = Arguments.New.State.mergeModelData mergedData model.newArgumentModel
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newArgumentModel = Arguments.New.State.setContext authentication language model.newArgumentModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewArgumentMsg (Arguments.New.State.subscriptions model.newArgumentModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebatePropertiesRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        DebatePropertiesRetrieved (Ok { data }) ->
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
                    { description = I18n.translate language I18n.ValuesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Values
                    }
                )

        NewArgumentMsg childMsg ->
            let
                ( updatedNewArgumentModel, childCmd ) =
                    Arguments.New.State.update childMsg model.newArgumentModel
            in
                ( { model | newArgumentModel = updatedNewArgumentModel }
                , Cmd.map translateNewArgumentMsg childCmd
                )

        Rate statementId rating ->
            ( model
            , case rating of
                Just rating ->
                    Requests.rateStatement model.authentication statementId rating
                        |> Http.send (ForSelf << RatingPosted)

                Nothing ->
                    Requests.unrateStatement model.authentication statementId
                        |> Http.send (ForSelf << RatingPosted)
            )

        RatingPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        RatingPosted (Ok { data }) ->
            ( mergeModelData data model
            , if data.id == model.id then
                Cmd.none
              else
                -- The rating of an argument may have changed => the affirmation rating may also have changed.
                -- => Retrieve it.
                Task.attempt (ForSelf << ValueUpdated)
                    (Process.sleep (1 * Time.second)
                        |> Task.andThen
                            (\_ ->
                                Requests.getValue
                                    model.authentication
                                    model.id
                                    |> Http.toTask
                            )
                    )
            )

        Retrieve ->
            ( { model
                | debatePropertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        Trash statementId ->
            ( model
            , Requests.postProperty model.authentication statementId "trashed" "true" Nothing
                |> Http.send (ForSelf << TrashUpserted)
            )

        TrashUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TrashUpserted (Ok body) ->
            ( model
            , Task.perform
                (\_ -> ForParent <| Navigate <| Urls.languagePath model.language ("/properties/" ++ body.data.id))
                (Task.succeed ())
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

        ValueRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        ValueRetrieved (Ok { data }) ->
            ( mergeModelData data model
            , Requests.getObjectProperties model.authentication model.showTrashed model.id debateKeyIds []
                |> Http.send (ForSelf << DebatePropertiesRetrieved)
            )

        ValueUpdated (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        ValueUpdated (Ok { data }) ->
            ( mergeModelData data model
            , Cmd.none
            )


urlUpdate : Navigation.Location -> Route -> Model -> ( Model, Cmd Msg )
urlUpdate location route model =
    let
        authentication =
            model.authentication

        id =
            model.id

        language =
            model.language

        unroutedModel =
            { model
              -- | argumentsModel = Nothing
              -- , sameKeyPropertiesModel = Nothing
                | showTrashed = Urls.queryToggle "trashed" location
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            IndexRoute ->
                ( updatedModel, updatedCmd )
