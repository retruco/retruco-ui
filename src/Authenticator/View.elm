module Authenticator.View exposing (..)

import Authenticator.Activate.View
import Authenticator.ChangePassword.View
import Authenticator.ResetPassword.View
import Authenticator.Routes exposing (Route(..))
import Authenticator.SignIn.View
import Authenticator.SignOut.View
import Authenticator.SignUp.View
import Authenticator.Types exposing (..)
import Html exposing (Html, text)
import I18n


modalTitle : I18n.Language -> Route -> String
modalTitle language route =
    case route of
        ActivateRoute _ ->
            (I18n.translate language I18n.ActivationTitle)

        ChangePasswordRoute _ ->
            (I18n.translate language I18n.ChangePassword)

        ResetPasswordRoute ->
            (I18n.translate language I18n.PasswordLost)

        SignInRoute ->
            (I18n.translate language I18n.SignInToContribute)

        SignOutRoute ->
            (I18n.translate language I18n.SignOutAndContributeLater)

        SignUpRoute ->
            (I18n.translate language I18n.CreateYourAccount)


view : Route -> Model -> Html Msg
view route model =
    case route of
        ActivateRoute _ ->
            Html.map
                translateActivateMsg
                (Authenticator.Activate.View.view model.language model.activate)

        ChangePasswordRoute _ ->
            Html.map
                translateChangePasswordMsg
                (Authenticator.ChangePassword.View.view model.language model.changePassword)

        ResetPasswordRoute ->
            Html.map
                translateResetPasswordMsg
                (Authenticator.ResetPassword.View.view model.language model.resetPassword)

        SignInRoute ->
            Html.map
                translateSignInMsg
                (Authenticator.SignIn.View.view model.language model.signIn)

        SignOutRoute ->
            Html.map
                translateSignOutMsg
                (Authenticator.SignOut.View.view model.language model.signOut)

        SignUpRoute ->
            Html.map
                translateSignUpMsg
                (Authenticator.SignUp.View.view model.language model.signUp)


viewModalBody : Route -> Model -> Html Msg
viewModalBody route model =
    case route of
        ActivateRoute _ ->
            Html.map
                translateActivateMsg
                (Authenticator.Activate.View.viewModalBody model.language model.activate)

        ChangePasswordRoute _ ->
            Html.map
                translateChangePasswordMsg
                (Authenticator.ChangePassword.View.viewModalBody model.language model.changePassword)

        ResetPasswordRoute ->
            Html.map
                translateResetPasswordMsg
                (Authenticator.ResetPassword.View.viewModalBody model.language model.resetPassword)

        SignInRoute ->
            Html.map
                translateSignInMsg
                (Authenticator.SignIn.View.viewModalBody model.language model.signIn)

        SignOutRoute ->
            Html.map
                translateSignOutMsg
                (Authenticator.SignOut.View.viewModalBody model.language model.signOut)

        SignUpRoute ->
            Html.map
                translateSignUpMsg
                (Authenticator.SignUp.View.viewModalBody model.language model.signUp)
