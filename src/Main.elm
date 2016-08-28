module Main exposing (..)

import Authenticator.Model
import Authenticator.Update
import Authenticator.View
-- import Combine exposing (Parser)
import Hop.Types
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Navigation
import Routes exposing (makeUrl, Route(..), urlParser)
import Statements
import Views exposing (aForPath)


main : Program Never
main =
    Navigation.program urlParser
        { init = init
        , update = update
        , urlUpdate = urlUpdate
        , view = view
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { authenticationMaybe : Maybe Authenticator.Model.Authentication
    , authenticator : Authenticator.Model.Model
    , location : Hop.Types.Location
    , page : String
    , route : Route
    , statements : Statements.Model
    }


init : ( Route, Hop.Types.Location ) -> ( Model, Cmd Msg )
init ( route, location ) =
    { authenticationMaybe = Nothing
    , authenticator = Authenticator.Model.init
    , location = location
    , page = "reference"
    , route = route
    , statements = Statements.init
    }
        |> urlUpdate ( route, location )


-- ROUTING


urlUpdate : ( Route, Hop.Types.Location ) -> Model -> ( Model, Cmd Msg )
urlUpdate ( route, location ) model =
    let
        updatedModel =
            { model | route = route, location = location }

        cmd =
            case route of
                -- Documentation ->
                --   Cmd.map Docs (Documentation.load "index")
                -- DocumentationPage page ->
                --   Cmd.map Docs (Documentation.load page)

                StatementsRoute ->
                    Cmd.map translateStatementsMsg Statements.load

                _ ->
                    Cmd.none
    in
        ( updatedModel, cmd )


-- UPDATE


type Msg
    = AuthenticatorMsg Authenticator.Update.Msg
    | Navigate String
    | StatementsMsg Statements.InternalMsg


statementsMsgTranslation : Statements.MsgTranslation Msg
statementsMsgTranslation =
    { onInternalMsg = StatementsMsg
    , onNavigate = Navigate
    }


translateStatementsMsg : Statements.MsgTranslator Msg
translateStatementsMsg = Statements.translateMsg statementsMsgTranslation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate path ->
            let
                command =
                    makeUrl path
                        |> Navigation.newUrl
            in
                ( model, command )

        AuthenticatorMsg subMsg ->
            let
                ( authenticator, subEffect ) =
                    Authenticator.Update.update subMsg model.authenticator
                changed = authenticator.authenticationMaybe /= model.authenticationMaybe
                model' = { model
                    | authenticationMaybe = authenticator.authenticationMaybe
                    , authenticator = authenticator
                    }
                (model'', effect'') = if changed
                    then
                        update (Navigate "/") model'
                    else
                        (model', Cmd.none)
            in
                model'' ! [Cmd.map AuthenticatorMsg subEffect, effect'']

        StatementsMsg subMsg ->
            let
                ( statements, subEffect ) =
                    Statements.update subMsg model.authenticationMaybe model.statements
            in
                ( { model | statements = statements }, Cmd.map translateStatementsMsg subEffect )


-- VIEW


view : Model -> Html Msg
view model =
    let
        profileNavItem = case model.authenticationMaybe of
            Just authentication ->
                li [] [ aForPath Navigate "/profile" [] [ text authentication.name ] ]
            Nothing ->
                text ""
        signInOrOutNavItem = case model.authenticationMaybe of
            Just authentication ->
                li [] [ aForPath Navigate "/sign_out" [] [ text "Sign Out" ] ]
            Nothing ->
                li [] [ aForPath Navigate "/sign_in" [] [ text "Sign In" ] ]
        signUpNavItem = case model.authenticationMaybe of
            Just authentication ->
                text ""
            Nothing ->
                li [] [ aForPath Navigate "/sign_up" [] [ text "Sign Up" ] ]
    in
        div
            [ class "container-fluid" ]
            (
                [ nav [class "navbar navbar-fixed-top navbar-inverse"]
                    [ div
                        [ class "container-fluid" ]
                        [ div
                            [ class "navbar-header" ]
                            [ button
                                [ ariaExpanded "false"
                                , class "navbar-toggle collapsed"
                                , attribute "data-target" "#navbar-collapse"
                                , attribute "data-toggle" "collapse"
                                , type' "button"
                                ]
                                [ span [ class "sr-only" ] [ text "Toggle navigation" ]
                                , span [ class "icon-bar" ] []
                                , span [ class "icon-bar" ] []
                                , span [ class "icon-bar" ] []
                                ]
                            , aForPath Navigate "/" [ class "navbar-brand"] [ text "Retruco" ]
                            ]
                        , div
                            [ class "collapse navbar-collapse"
                            , id "navbar-collapse"
                            ]
                            [ ul [ class "nav navbar-nav" ]
                                [ li [] [ aForPath Navigate "/about" [] [ text "About" ] ]
                                , li [] [ aForPath Navigate "/statements" [] [ text "Statements" ] ]
                                ]
                            , ul [ class "nav navbar-nav navbar-right" ]
                                [ profileNavItem
                                , signInOrOutNavItem
                                , signUpNavItem
                                ]
                            ]
                        ]
                    ]
                ]
                ++ [ viewContent model ]
            )


viewContent : Model -> Html Msg
viewContent model =
    case model.route of
        --   Home ->
        --     Pages.Index.view Navigate
        --   ReferencePage ->
        --     Html.App.map Reference (Reference.view model.reference "")
        --   Component comp ->
        --     Html.App.map Reference (Reference.view model.reference comp)
        --   Documentation ->
        --     Html.App.map Docs (Documentation.view model.docs)
        --   DocumentationPage page ->
        --     Html.App.map Docs (Documentation.view model.docs)
        AuthenticatorRoute subRoute ->
            Html.App.map AuthenticatorMsg (Authenticator.View.view subRoute model.authenticator)
        StatementsRoute ->
            Html.App.map translateStatementsMsg (Statements.view model.authenticationMaybe model.statements)

        _ ->
            p
                []
                [ img [ src "./img/elm.png" ] []
                , text "Hello world"
                ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -- Sub.batch
    --     -- [ Emitter.listenString "navigation" Navigate
    --     -- , Sub.map Reference (Reference.subscriptions model.reference)
    --     ]
