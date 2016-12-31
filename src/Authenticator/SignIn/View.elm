module Authenticator.SignIn.View exposing (..)

import Authenticator.Routes exposing (..)
import Authenticator.SignIn.Types exposing (..)
import Authenticator.ViewsParts exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(BadStatus))
import I18n
import Views exposing (getHttpErrorAsString)


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
                                I18n.translate language I18n.AuthenticationFailed
                                    ++ I18n.translate language I18n.Colon
                            ]
                        , text
                            (case httpError of
                                BadStatus response ->
                                    if response.status.code == 401 then
                                        I18n.translate language I18n.BadEmailOrPassword
                                    else
                                        getHttpErrorAsString language httpError

                                _ ->
                                    getHttpErrorAsString language httpError
                            )
                        ]
                    ]
    in
        Html.form [ onSubmit (ForSelf <| Submit) ]
            (alert
                ++ [ viewEmailControl
                        (ForSelf << EmailInput)
                        language
                        (Dict.get "email" model.errors)
                        model.email
                   , viewPasswordControl
                        (ForSelf << PasswordInput)
                        language
                        (Dict.get "password" model.errors)
                        model.password
                   , div [ class "form-group" ]
                        [ button
                            [ class "btn btn-primary"
                            , type_ "submit"
                            ]
                            [ text (I18n.translate language I18n.SignIn) ]
                        , text " "
                        , button
                            [ class "btn btn-secondary"
                            , type_ "button"
                            , onClick (ForParent <| ChangeRoute ResetPasswordRoute)
                            ]
                            [ text (I18n.translate language I18n.ResetPassword) ]
                        , text " "
                        , button
                            [ class "btn btn-secondary"
                            , type_ "button"
                            , onClick (ForParent <| ChangeRoute SignUpRoute)
                            ]
                            [ text (I18n.translate language I18n.SignUp) ]
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