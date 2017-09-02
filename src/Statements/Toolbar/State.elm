module Statements.Toolbar.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Ports
import Requests
import Statements.Toolbar.Types exposing (..)
import Task
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> DataProxy {} -> Statement b -> Model (Statement b)
init authentication language data statement =
    { authentication = authentication
    , data = data
    , httpError = Nothing
    , language = language
    , statement = statement
    }


setContext : Maybe Authentication -> I18n.Language -> Model (Statement b) -> Model (Statement b)
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
    }


setData : DataProxy {} -> Statement b -> Model (Statement b) -> Model (Statement b)
setData data statement model =
    -- Note: setData is used when the content of a stetement changes. It must not be used when the statement its (aka
    -- its id) changes.
    { model | data = data, statement = statement }


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

        Trash ->
            ( model
            , Requests.postProperty model.authentication model.statement.id "trashed" "true" Nothing
                |> Http.send (ForSelf << TrashPosted)
            )

        TrashPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TrashPosted (Ok body) ->
            ( model
            , Task.perform
                (\_ ->
                    ForParent <|
                        Navigate <|
                            Urls.languagePath model.language <|
                                Urls.idToPropertiesPath body.data body.data.id
                )
                (Task.succeed ())
            )
