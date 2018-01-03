module Statements.Toolbar.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Data exposing (initData, mergeData)
import Http
import I18n
import Ports
import Process
import Requests
import Statements.Toolbar.Types exposing (..)
import Task
import Time
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> Data -> Statement b -> Model (Statement b)
init authentication embed language data statement =
    { authentication = authentication
    , data = data
    , embed = embed
    , httpError = Nothing
    , language = language
    , statement = statement
    , trashAction = Nothing
    , trashPropertyId = Nothing
    }


propagateModelDataChange : Model (Statement b) -> Model (Statement b)
propagateModelDataChange model =
    model


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model (Statement b) -> Model (Statement b)
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
    }


setModelData : Data -> Statement b -> Model (Statement b) -> Model (Statement b)
setModelData data statement model =
    -- Note: setModelData is used when the content of a stetement changes. It must not be used when the statement (aka
    -- its id) changes.
    { model
        | data = data
        , statement = statement
    }


update : InternalMsg -> Model (Statement b) -> ( Model (Statement b), Cmd Msg )
update msg model =
    case msg of
        Rate rating ->
            ( model
            , case rating of
                Just rating ->
                    Requests.rateStatement model.authentication model.statement.id rating
                        |> Http.send (ForSelf << RatingPosted)

                Nothing ->
                    Requests.unrateStatement model.authentication model.statement.id
                        |> Http.send (ForSelf << RatingPosted)
            )

        RatingPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        RatingPosted (Ok { data }) ->
            -- Note: Don't merge data with model.data, because it will be done by parent when receiving the DataUpdated
            -- message.
            ( model
            , Task.perform (\_ -> ForParent <| DataUpdated <| mergeData data initData) (Task.succeed ())
            )

        ShareOnFacebook url ->
            ( model, Ports.shareOnFacebook url )

        ShareOnGooglePlus url ->
            ( model, Ports.shareOnGooglePlus url )

        ShareOnLinkedIn url ->
            ( model, Ports.shareOnLinkedIn url )

        ShareOnTwitter url ->
            ( model, Ports.shareOnTwitter url )

        Start ->
            ( model
            , Requests.getProperties model.authentication True [ model.statement.id ] [ "trashed" ] [ "true" ]
                |> Http.send (ForSelf << TrashRetrieved)
            )

        StatementRetrieved (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        StatementRetrieved (Ok { data }) ->
            ( model
            , Task.perform (\_ -> ForParent <| DataUpdated <| mergeData data initData) (Task.succeed ())
            )

        Trash trashAction ->
            let
                trashRating =
                    case trashAction of
                        DebateTrash ->
                            Nothing

                        RateTrash (Just rating) ->
                            Just rating

                        RateTrash Nothing ->
                            Nothing
            in
                ( { model
                    | trashAction =
                        case trashAction of
                            DebateTrash ->
                                Just trashAction

                            RateTrash (Just rating) ->
                                Nothing

                            RateTrash Nothing ->
                                Just trashAction
                  }
                , Requests.postProperty model.authentication model.statement.id "trashed" "true" trashRating
                    |> Http.send (ForSelf << TrashPosted)
                )

        TrashPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TrashPosted (Ok { data }) ->
            let
                trashPropertyId =
                    data.id
            in
                { model | trashPropertyId = Just trashPropertyId }
                    ! [ Task.perform (\_ -> ForParent <| DataUpdated <| mergeData data initData) (Task.succeed ())
                      , case model.trashAction of
                            Just DebateTrash ->
                                Task.perform
                                    (\_ ->
                                        ForParent <|
                                            Navigate <|
                                                Urls.languagePath model.embed model.language <|
                                                    Urls.idToPropertiesPath data trashPropertyId
                                    )
                                    (Task.succeed ())

                            Just (RateTrash (Just trashRating)) ->
                                Requests.rateStatement model.authentication trashPropertyId trashRating
                                    |> Http.send (ForSelf << TrashRatingPosted)

                            Just (RateTrash Nothing) ->
                                Requests.unrateStatement model.authentication trashPropertyId
                                    |> Http.send (ForSelf << TrashRatingPosted)

                            Nothing ->
                                Task.attempt (ForSelf << StatementRetrieved)
                                    (Process.sleep (1 * Time.second)
                                        |> Task.andThen
                                            (\_ ->
                                                Requests.getValue model.authentication model.statement.id
                                                    |> Http.toTask
                                            )
                                    )
                      ]

        TrashRatingPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TrashRatingPosted (Ok { data }) ->
            -- Note: Don't merge data with model.data, because it will be done by parent when receiving the DataUpdated
            -- message.
            model
                ! [ Task.perform (\_ -> ForParent <| DataUpdated <| mergeData data initData) (Task.succeed ())
                  , Task.attempt (ForSelf << StatementRetrieved)
                        (Process.sleep (1 * Time.second)
                            |> Task.andThen
                                (\_ ->
                                    Requests.getValue model.authentication model.statement.id
                                        |> Http.toTask
                                )
                        )
                  ]

        TrashRetrieved (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TrashRetrieved (Ok { data }) ->
            ( { model | trashPropertyId = Array.get 0 data.ids }
            , Task.perform (\_ -> ForParent <| DataUpdated <| mergeData data initData) (Task.succeed ())
            )
