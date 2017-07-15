module Root.View exposing (..)

import Assertions.Index.View
import Assertions.Item.View
import Assertions.New.View
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
                                -- [ li [ class "nav-item" ]
                                --     [ aForPath Navigate
                                --         language
                                --         "/cards"
                                --         [ class "nav-link" ]
                                --         [ text <| I18n.translate language I18n.Cards ]
                                --     ]
                                [ li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/assertions"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Assertions ]
                                    ]

                                -- , li [ class "nav-item" ]
                                --     [ aForPath Navigate
                                --         language
                                --         "/values"
                                --         [ class "nav-link" ]
                                --         [ text <| I18n.translate language I18n.Values ]
                                --     ]
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

                        AssertionsRoute childRoute ->
                            case childRoute of
                                AssertionRoute _ _ ->
                                    case model.assertionModel of
                                        Just assertionModel ->
                                            Assertions.Item.View.view assertionModel
                                                |> Html.map translateAssertionMsg

                                        Nothing ->
                                            text "This should not occur: assertionModel == Nothing!"

                                AssertionsIndexRoute ->
                                    case model.assertionsModel of
                                        Just assertionsModel ->
                                            Assertions.Index.View.view assertionsModel
                                                |> Html.map translateAssertionsMsg

                                        Nothing ->
                                            text "This should not occur: assertionsModel == Nothing!"

                                NewAssertionRoute ->
                                    case model.newAssertionModel of
                                        Just newAssertionModel ->
                                            Assertions.New.View.view newAssertionModel
                                                |> Html.map translateNewAssertionMsg

                                        Nothing ->
                                            text "This should not occur: newAssertionModel == Nothing!"

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
                                    case model.cardsModel of
                                        Just cardsModel ->
                                            Cards.Index.View.view cardsModel
                                                |> Html.map translateCardsMsg

                                        Nothing ->
                                            text "This should not occur: cardsModel == Nothing!"

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

                                ValueRoute _ _ ->
                                    case model.valueModel of
                                        Just valueModel ->
                                            Values.Item.View.view valueModel
                                                |> Html.map translateValueMsg

                                        Nothing ->
                                            text "This should not occur: valueModel == Nothing!"

                                ValuesIndexRoute ->
                                    case model.valuesModel of
                                        Just valuesModel ->
                                            Values.Index.View.view valuesModel
                                                |> Html.map translateValuesMsg

                                        Nothing ->
                                            text "This should not occur: valuesModel == Nothing!"
                    ]

        I18nRouteWithoutLanguage _ ->
            div [ style [ ( "min-height", "60em" ) ] ]
                [ viewBigMessage "" "" ]



-- StatementsRoute nestedRoute ->
--     Html.map translateStatementsMsg (Statements.view model.authentication model.statementsModel)
