module Root.View exposing (..)

import Authenticator.View
import Card.View
import Cards.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import NewValue.View
import Root.Types exposing (..)
import Routes exposing (..)
import Search
import Value.View
import Values.View
import Views exposing (..)


view : Model -> Html Msg
view model =
    case model.route of
        (I18nRouteWithLanguage language localizedRoute) as route ->
            let
                profileNavItem =
                    case model.authentication of
                        Just authentication ->
                            li [ class "nav-item" ]
                                [ aForPath
                                    Navigate
                                    language
                                    "/profile"
                                    [ class "nav-link" ]
                                    [ text authentication.name ]
                                ]

                        Nothing ->
                            text ""

                signInOrOutNavItem =
                    case model.authentication of
                        Just authentication ->
                            li [ class "nav-item" ]
                                [ aForPath
                                    Navigate
                                    language
                                    "/sign_out"
                                    [ class "nav-link" ]
                                    [ text <| I18n.translate language I18n.SignOut ]
                                ]

                        Nothing ->
                            li [ class "nav-item" ]
                                [ aForPath
                                    Navigate
                                    language
                                    "/sign_in"
                                    [ class "nav-link" ]
                                    [ text <| I18n.translate language I18n.SignIn ]
                                ]

                signUpNavItem =
                    case model.authentication of
                        Just authentication ->
                            text ""

                        Nothing ->
                            li [ class "nav-item" ]
                                [ aForPath
                                    Navigate
                                    language
                                    "/sign_up"
                                    [ class "nav-link" ]
                                    [ text <| I18n.translate language I18n.SignUp ]
                                ]
            in
                div
                    [ class "container-fluid" ]
                    [ nav [ class "navbar navbar-fixed-top navbar-dark bg-inverse" ]
                        [ aForPath Navigate language "/" [ class "navbar-brand" ] [ text "Retruco.org" ]
                        , button
                            [ attribute "aria-controls" "collapsable-navbar"
                            , attribute "aria-expanded" "false"
                            , attribute "aria-label" "Toggle navigation"
                            , class "navbar-toggler float-xs-right hidden-lg-up"
                            , attribute "data-target" "#collapsable-navbar"
                            , attribute "data-toggle" "collapse"
                            , type_ "button"
                            ]
                            []
                        , div [ class "collapse navbar-toggleable-md", id "collapsable-navbar" ]
                            [ ul [ class "nav navbar-nav" ]
                                -- [ li [ class "nav-item" ]
                                --     [ aForPath Navigate
                                --         language
                                --         "/"
                                --         [ class "nav-link" ]
                                --         [ span [ class "fa fa-search" ] []
                                --         , text " "
                                --         , text <| I18n.translate language I18n.Search
                                --         ]
                                --     ]
                                -- , li [ class "nav-item" ]
                                --     [ aForPath
                                --         Navigate
                                --         language
                                --         "/about"
                                --         [ class "nav-link" ]
                                --         [ text <| I18n.translate language I18n.About ]
                                --     ]
                                [ li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/cards"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Cards ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/concepts"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Concepts ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/values"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Values ]
                                    ]
                                ]
                            , ul [ class "nav navbar-nav float-xs-right" ]
                                [ profileNavItem
                                , signInOrOutNavItem
                                , signUpNavItem
                                ]
                            , span [ class "navbar-text float-xs-right" ] [ text "   " ]
                            , Html.form [ class "form-inline float-xs-right" ]
                                [ input
                                    [ class "form-control"
                                    , placeholder <| I18n.translate language I18n.SearchPlaceholder
                                    , type_ "text"
                                    ]
                                    []
                                , button [ class "btn btn-outline-success", type_ "button" ]
                                    [ text <| I18n.translate language I18n.Search ]
                                ]
                            ]
                        ]
                    , case localizedRoute of
                        AboutRoute ->
                            p
                                []
                                [ img [ src "./img/elm.png" ] []
                                , text "About Retruco"
                                ]

                        AuthenticatorRoute childRoute ->
                            Authenticator.View.view childRoute model.authenticatorModel
                                |> Html.map translateAuthenticatorMsg

                        CardsRoute childRoute ->
                            case childRoute of
                                CardRoute _ ->
                                    Card.View.view model.cardModel
                                        |> Html.map translateCardMsg

                                CardsIndexRoute ->
                                    Cards.View.view model.cardsModel
                                        |> Html.map translateCardsMsg

                        -- NewCardRoute ->
                        --     NewCard.View.view model.newCardModel
                        --         |> Html.map translateNewCardMsg
                        NotFoundRoute _ ->
                            viewNotFound language

                        SearchRoute ->
                            div []
                                [ Html.map translateSearchMsg
                                    (Search.view model.authentication language model.searchModel)
                                  -- , Html.map
                                  --     translateStatementsMsg
                                  --     (Statements.viewIndex model.authentication model.statementsModel)
                                ]

                        UserProfileRoute ->
                            viewNotFound language

                        ValuesRoute childRoute ->
                            case childRoute of
                                NewValueRoute ->
                                    NewValue.View.view model.newValueModel
                                        |> Html.map translateNewValueMsg

                                ValueRoute _ ->
                                    Value.View.view model.valueModel
                                        |> Html.map translateValueMsg

                                ValuesIndexRoute ->
                                    Values.View.view model.valuesModel
                                        |> Html.map translateValuesMsg
                    ]

        I18nRouteWithoutLanguage _ ->
            div [ style [ ( "min-height", "60em" ) ] ]
                [ viewBigMessage "" "" ]



-- StatementsRoute nestedRoute ->
--     Html.map translateStatementsMsg (Statements.view model.authentication model.statementsModel)
