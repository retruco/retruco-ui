module Main exposing (..)


import Authenticator.Model
import Authenticator.Update
import Authenticator.View
import Combine exposing (Parser)
import Hop
import Hop.Matchers exposing (match1, match2)
import Hop.Types
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Navigation
-- import Statements


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
    -- , statements : Statements.Model
    }


init : ( Route, Hop.Types.Location ) -> ( Model, Cmd Msg )
init ( route, location ) =
    { authenticationMaybe = Nothing
    , authenticator = Authenticator.Model.init
    , location = location
    , page = "reference"
    , route = route
    -- , statements = Statements.init
    }
        |> urlUpdate ( route, location )


-- ROUTING


type Route
    = Component String
    | Home
    | AuthenticatorRoute Authenticator.Model.Route
    | StatementsRoute
    | Documentation
    | DocumentationPage String
    | ReferencePage


all : Parser String
all =
    Combine.regex ".+"


idParser : Parser String
idParser =
    Combine.regex ".+"


makeUrl : String -> String
makeUrl path = Hop.makeUrl routerConfig path 


matchers : List (Hop.Types.PathMatcher Route)
matchers =
    [ match1 Home ""
    , match2 Component "/reference/" all
    , match1 Documentation "/documentation"
    , match2 DocumentationPage "/documentation/" all
    , match1 ReferencePage "/reference"
    , match1 (AuthenticatorRoute Authenticator.Model.SignInRoute) "/sign_in"
    , match1 (AuthenticatorRoute Authenticator.Model.SignOutRoute) "/sign_out"
    , match1 (AuthenticatorRoute Authenticator.Model.SignUpRoute) "/sign_up"
    , match1 StatementsRoute "/statements"
      -- , match2 Statement "/statements/" idParser
    ]


urlParser : Navigation.Parser ( Route, Hop.Types.Location )
urlParser =
    Navigation.makeParser (.href >> Hop.matchUrl routerConfig)


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

                -- StatementsRoute ->
                --     Cmd.map StatementsMsg Statements.load

                _ ->
                    Cmd.none
    in
        ( updatedModel, cmd )


routerConfig : Hop.Types.Config Route
routerConfig =
    -- Production:
    -- { hash = False
    -- , basePath = ""
    -- , matchers = matchers
    -- , notFound = Home
    -- }
    -- Development:
    { hash = True
    , basePath = ""
    , matchers = matchers
    , notFound = Home
    }


-- UPDATE


type Msg
    = AuthenticatorMsg Authenticator.Update.Msg
    | Navigate String
    -- | StatementsMsg Statements.Msg


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
                -- (model'', effect'') = case authenticationMaybe of
                --     Just authentication ->
                --         update (Navigate "/") model'
                --     Nothing ->
                --         (model', Cmd.none)
                (model'', effect'') = if changed
                    then
                        update (Navigate "/") model'
                    else
                        (model', Cmd.none)
            in
                model'' ! [Cmd.map AuthenticatorMsg subEffect, effect'']

        -- StatementsMsg subMsg ->
        --     let
        --         ( statements, effect ) =
        --             Statements.update subMsg model.statements
        --     in
        --         ( { model | statements = statements }, Cmd.map StatementsMsg effect )


-- VIEW


aForPath : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
aForPath path attributes children =
    a
        (
            [ href (makeUrl path)
            , onWithOptions
                "click"
                { stopPropagation = False, preventDefault = True }
                (Json.Decode.succeed (Navigate path))
            ]
            ++ attributes
        )
        children


view : Model -> Html Msg
view model =
    let
        profileNavItem = case model.authenticationMaybe of
            Just authentication ->
                li [] [ aForPath "/profile" [] [ text authentication.name ] ]
            Nothing ->
                text ""
        signInOrOutNavItem = case model.authenticationMaybe of
            Just authentication ->
                li [] [ aForPath "/sign_out" [] [ text "Sign Out" ] ]
            Nothing ->
                li [] [ aForPath "/sign_in" [] [ text "Sign In" ] ]
        signUpNavItem = case model.authenticationMaybe of
            Just authentication ->
                text ""
            Nothing ->
                li [] [ aForPath "/sign_up" [] [ text "Sign Up" ] ]
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
                            , aForPath "/" [ class "navbar-brand"] [ text "TODO: config.title" ]
                            ]
                        , div
                            [ class "collapse navbar-collapse"
                            , id "navbar-collapse"
                            ]
                            [ ul [ class "nav navbar-nav" ]
                                [ li [] [ aForPath "/about" [] [ text "About" ] ]
                                , li [] [ aForPath "/statements" [] [ text "Statements" ] ]
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


-- view : Model -> Html Msg
-- view model =
--     Ui.App.view
--         App
--         model.app
--         [ Ui.Header.view []
--             ([ Ui.Header.title [ onClick (Navigate "/") ]
--                 [ img [ src "/images/logo-small.svg" ] []
--                 , text "Retruco"
--                 ]
--              ]
--                 ++ (List.map viewHeaderPageLink pages)
--                 ++ [ viewHeaderExternalLink "Github" "social-github" "https://github.com/gdotdesign/elm-ui"
--                    ]
--             )
--         , viewContent model
--         , node "ui-footer" [] []
--         ]


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

        -- StatementsRoute ->
        --     Html.App.map StatementsMsg (Statements.view model.statements)

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
