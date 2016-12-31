module Authenticator.ChangePassword.View exposing (..)

import Authenticator.ChangePassword.Types exposing (..)
import Authenticator.ViewsParts exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(BadStatus))
import Http.Error
import I18n


view : I18n.Language -> Model -> Html Msg
view language model =
    let
        alert =
            case model.httpError of
                Nothing ->
                    []

                Just httpError ->
                    [ div
                        [ class "alert alert-danger"
                        , role "alert"
                        ]
                        [ strong []
                            [ text <|
                                I18n.translate language I18n.PasswordChangeFailed
                                    ++ I18n.translate language I18n.Colon
                            ]
                        , text
                            (case httpError of
                                BadStatus response ->
                                    if response.status.code == 400 then
                                        I18n.translate language I18n.BadAuthorization
                                    else if response.status.code == 404 then
                                        I18n.translate language I18n.UnknownUser
                                    else
                                        Http.Error.toString language httpError

                                _ ->
                                    Http.Error.toString language httpError
                            )
                        ]
                    ]
    in
        Html.form [ onSubmit (ForSelf <| Submit) ]
            (alert
                ++ [ viewPasswordControl
                        (ForSelf << PasswordInput)
                        language
                        (Dict.get "password" model.errors)
                        model.password
                   , div [ class "form-group" ]
                        [ button
                            [ class "btn btn-primary"
                            , type_ "submit"
                            ]
                            [ text (I18n.translate language I18n.Save) ]
                        , text " "
                        , button
                            [ class "btn btn-warning float-xs-right"
                            , type_ "button"
                            , onClick (ForSelf <| Cancel)
                            ]
                            [ text (I18n.translate language I18n.Cancel) ]
                        ]
                   ]
            )


viewModalBody : I18n.Language -> Model -> Html Msg
viewModalBody language model =
    div [ class "modal-body" ]
        [ view language model ]
