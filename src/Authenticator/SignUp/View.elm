module Authenticator.SignUp.View exposing (..)

import Authenticator.Routes exposing (..)
import Authenticator.SignUp.Types exposing (..)
import Authenticator.ViewsHelpers exposing (..)
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
                                I18n.translate language I18n.AccountCreationFailed
                                    ++ I18n.translate language I18n.Colon
                            ]
                        , text
                            (case httpError of
                                BadStatus response ->
                                    if response.status.code == 400 then
                                        I18n.translate language I18n.UsernameOrEmailAlreadyExist
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
                ++ [ viewUsernameControl
                        (ForSelf << UsernameInput)
                        language
                        (Dict.get "username" model.errors)
                        model.username
                   , viewEmailControl
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
                            [ text (I18n.translate language I18n.Register) ]
                        , text " "
                        , button
                            [ class "btn btn-secondary"
                            , type_ "button"
                            , onClick (ForParent <| ChangeRoute SignInRoute)
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
