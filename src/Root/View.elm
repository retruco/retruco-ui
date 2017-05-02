module Root.View exposing (..)

import Authenticator.View
import Cards.Index.View
import Cards.Item.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Root.Types exposing (..)
import Routes exposing (..)
import Search
import Values.Index.View
import Values.Item.View
import Values.New.View
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
                    [ nav [ class "navbar navbar-fixed-top navbar-inverse navbar-toggleable-md bg-inverse" ]
                        [ aForPath Navigate language "/" [ class "navbar-brand" ] [ text "Retruco.org" ]
                        , button
                            [ attribute "aria-controls" "collapsable-navbar"
                            , attribute "aria-expanded" "false"
                            , attribute "aria-label" "Toggle navigation"
                            , class "navbar-toggler navbar-toggler-right"
                            , attribute "data-target" "#collapsable-navbar"
                            , attribute "data-toggle" "collapse"
                            , type_ "button"
                            ]
                            [ span [ class "navbar-toggler-icon" ] [] ]
                        , div [ class "collapse navbar-collapse", id "collapsable-navbar" ]
                            [ ul [ class "navbar-nav mr-auto" ]
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
                            , ul [ class "navbar-nav" ]
                                [ profileNavItem
                                , signInOrOutNavItem
                                , signUpNavItem
                                ]
                            , text " "
                            , Html.form [ class "form-inline" ]
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
                                CardRoute _ _ ->
                                    case model.cardModel of
                                        Just cardModel ->
                                            Cards.Item.View.view cardModel
                                                |> Html.map translateCardMsg

                                        Nothing ->
                                            text "This should not occur: cardModel == Nothing!"

                                CardsIndexRoute ->
                                    Cards.Index.View.view model.cardsModel
                                        |> Html.map translateCardsMsg

                        -- NewCardRoute ->
                        --     NewCards.Item.View.view model.newCardModel
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
                                    case model.newValueModel of
                                        Just newValueModel ->
                                            Values.New.View.view newValueModel
                                                |> Html.map translateNewValueMsg

                                        Nothing ->
                                            text "This should not occur: newValueModel == Nothing!"

                                ValueRoute _ ->
                                    Values.Item.View.view model.valueModel
                                        |> Html.map translateValueMsg

                                ValuesIndexRoute ->
                                    Values.Index.View.view model.valuesModel
                                        |> Html.map translateValuesMsg
                    ]

        I18nRouteWithoutLanguage _ ->
            div [ style [ ( "min-height", "60em" ) ] ]
                [ viewBigMessage "" "" ]



-- StatementsRoute nestedRoute ->
--     Html.map translateStatementsMsg (Statements.view model.authentication model.statementsModel)
