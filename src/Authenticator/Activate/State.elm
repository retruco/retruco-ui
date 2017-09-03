module Authenticator.Activate.State exposing (..)

import Authenticator.Activate.Types exposing (..)
import Http
import I18n
import Requests
import Task


init : Model
init =
    { authentication = Nothing
    , httpError = Nothing
    }


update : InternalMsg -> Model -> I18n.Language -> ( Model, Cmd Msg )
update msg model _ =
    case msg of
        ActivateUser userId authorization ->
            ( { model | httpError = Nothing }
            , Requests.activateUser userId authorization
                |> Http.send (ForSelf << UserActivated)
            )

        ActivationSent (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        ActivationSent (Ok _) ->
            ( model, Cmd.none )

        SendActivation authentication ->
            ( { model | httpError = Nothing }
            , Requests.sendActivation authentication
                |> Http.send (ForSelf << ActivationSent)
            )

        UserActivated (Err httpError) ->
            ( { model | httpError = Just httpError }
            , Task.perform (\_ -> ForParent (Terminated (Err ()))) (Task.succeed ())
            )

        UserActivated (Ok { data }) ->
            ( { model | authentication = Just data }
            , Task.perform (\_ -> ForParent (Terminated (Ok <| Just data))) (Task.succeed ())
            )
