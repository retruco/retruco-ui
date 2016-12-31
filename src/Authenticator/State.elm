module Authenticator.State exposing (..)

import Authenticator.Types exposing (Model)
import Authenticator.Activate.State
import Authenticator.Activate.Types
import Authenticator.ChangePassword.State
import Authenticator.ResetPassword.State
import Authenticator.Routes exposing (..)
import Authenticator.SignIn.State
import Authenticator.SignOut.State
import Authenticator.SignUp.State
import Authenticator.Types exposing (..)
import I18n
import Navigation
import Ports
import Urls


init : Model
init =
    { activate = Authenticator.Activate.State.init
    , changePassword = Authenticator.ChangePassword.State.init "" ""
    , language = I18n.English
    , resetPassword = Authenticator.ResetPassword.State.init
    , signIn = Authenticator.SignIn.State.init
    , signOut = Authenticator.SignOut.State.init
    , signUp = Authenticator.SignUp.State.init
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActivateMsg subMsg ->
            let
                ( activate, activateCmd ) =
                    Authenticator.Activate.State.update subMsg model.activate model.language

                model_ =
                    { model | activate = activate }
            in
                ( model_, Cmd.map translateActivateMsg activateCmd )

        ChangePasswordMsg subMsg ->
            let
                ( changePassword, changePasswordCmd ) =
                    Authenticator.ChangePassword.State.update subMsg model.changePassword model.language

                model_ =
                    { model | changePassword = changePassword }
            in
                ( model_, Cmd.map translateChangePasswordMsg changePasswordCmd )

        ResetPasswordMsg subMsg ->
            let
                ( resetPassword, resetPasswordCmd ) =
                    Authenticator.ResetPassword.State.update subMsg model.resetPassword

                model_ =
                    { model | resetPassword = resetPassword }
            in
                ( model_, Cmd.map translateResetPasswordMsg resetPasswordCmd )

        SignInMsg subMsg ->
            let
                ( signIn, signInCmd ) =
                    Authenticator.SignIn.State.update subMsg model.signIn

                model_ =
                    { model | signIn = signIn }
            in
                ( model_, Cmd.map translateSignInMsg signInCmd )

        SignOutMsg subMsg ->
            let
                ( signOut, signOutCmd ) =
                    Authenticator.SignOut.State.update subMsg model.signOut

                model_ =
                    { model | signOut = signOut }
            in
                ( model_, Cmd.map translateSignOutMsg signOutCmd )

        SignUpMsg subMsg ->
            let
                ( signUp, signUpCmd ) =
                    Authenticator.SignUp.State.update subMsg model.signUp

                model_ =
                    { model | signUp = signUp }
            in
                ( model_, Cmd.map translateSignUpMsg signUpCmd )


urlUpdate : I18n.Language -> Navigation.Location -> Route -> Model -> ( Model, Cmd Msg )
urlUpdate language location route model =
    case route of
        ActivateRoute userId ->
            let
                authorization =
                    Urls.querySingleParameter
                        "authorization"
                        location
                        |> Maybe.withDefault ""

                ( newModel, cmd ) =
                    update
                        (ActivateMsg <| Authenticator.Activate.Types.ActivateUser userId authorization)
                        { model | language = language }
            in
                newModel
                    ! [ cmd
                      , Ports.setDocumentMetadata
                            { description = I18n.translate language I18n.ActivationDescription
                            , imageUrl = Urls.appLogoFullUrl
                            , title = I18n.translate language I18n.ActivationTitle
                            }
                      ]

        ChangePasswordRoute userId ->
            let
                authorization =
                    Urls.querySingleParameter
                        "authorization"
                        location
                        |> Maybe.withDefault ""
            in
                ( { model
                    | changePassword = Authenticator.ChangePassword.State.init userId authorization
                    , language = language
                  }
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.ChangePasswordDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.ChangePasswordTitle
                    }
                )

        ResetPasswordRoute ->
            ( { model | language = language }
            , Ports.setDocumentMetadata
                { description = I18n.translate language I18n.ResetPasswordDescription
                , imageUrl = Urls.appLogoFullUrl
                , title = I18n.translate language I18n.ResetPasswordTitle
                }
            )

        SignInRoute ->
            ( { model | language = language }
            , Ports.setDocumentMetadata
                { description = I18n.translate language I18n.SignInDescription
                , imageUrl = Urls.appLogoFullUrl
                , title = I18n.translate language I18n.SignInTitle
                }
            )

        SignOutRoute ->
            ( { model | language = language }
            , Ports.setDocumentMetadata
                { description = I18n.translate language I18n.SignOutDescription
                , imageUrl = Urls.appLogoFullUrl
                , title = I18n.translate language I18n.SignOutTitle
                }
            )

        SignUpRoute ->
            ( { model | language = language }
            , Ports.setDocumentMetadata
                { description = I18n.translate language I18n.SignUpDescription
                , imageUrl = Urls.appLogoFullUrl
                , title = I18n.translate language I18n.SignUpTitle
                }
            )
