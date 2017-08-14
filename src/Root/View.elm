module Root.View exposing (..)

import About.View
import Affirmations.Index.View
import Affirmations.Item.View
import Affirmations.New.View
import Arguments.Item.View
import Authenticator.View
import Cards.Index.View
import Cards.Item.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Properties.Item.View
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
                    [ nav [ class "bg-dark fixed-top navbar navbar-dark navbar-expand-sm" ]
                        [ aForPath
                            Navigate
                            language
                            "/"
                            [ class "navbar-brand" ]
                            [ img
                                [ alt ""
                                , class "align-top d-inline-block"
                                , height 30
                                , src "/img/icon-30x30.png"
                                , width 30
                                ]
                                []
                            , text " "
                            , text "Retruco.org"
                            ]
                        , button
                            [ attribute "aria-controls" "collapsable-navbar"
                            , attribute "aria-expanded" "false"
                            , attribute "aria-label" "Toggle navigation"
                            , class "navbar-toggler"
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
                                [ li [ class "nav-item" ]
                                    [ aForPath
                                        Navigate
                                        language
                                        "/about"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.About ]
                                    ]

                                -- [ li [ class "nav-item" ]
                                --     [ aForPath Navigate
                                --         language
                                --         "/cards"
                                --         [ class "nav-link" ]
                                --         [ text <| I18n.translate language I18n.Cards ]
                                --     ]
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/affirmations"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Affirmations ]
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

                            -- , text " "
                            -- , Html.form [ class "form-inline" ]
                            --     [ input
                            --         [ class "form-control"
                            --         , placeholder <| I18n.translate language I18n.SearchPlaceholder
                            --         , type_ "text"
                            --         ]
                            --         []
                            --     , button [ class "btn btn-outline-success", type_ "button" ]
                            --         [ text <| I18n.translate language I18n.Search ]
                            --     ]
                            ]
                        ]
                    , case localizedRoute of
                        AboutRoute ->
                            case model.aboutModel of
                                Just aboutModel ->
                                    About.View.view aboutModel
                                        |> Html.map translateAboutMsg

                                Nothing ->
                                    text "This should not occur: aboutModel == Nothing!"

                        AffirmationsRoute childRoute ->
                            case childRoute of
                                AffirmationRoute _ _ ->
                                    case model.affirmationModel of
                                        Just affirmationModel ->
                                            Affirmations.Item.View.view affirmationModel
                                                |> Html.map translateAffirmationMsg

                                        Nothing ->
                                            text "This should not occur: affirmationModel == Nothing!"

                                AffirmationsIndexRoute ->
                                    case model.affirmationsModel of
                                        Just affirmationsModel ->
                                            Affirmations.Index.View.view affirmationsModel
                                                |> Html.map translateAffirmationsMsg

                                        Nothing ->
                                            text "This should not occur: affirmationsModel == Nothing!"

                                NewAffirmationRoute ->
                                    case model.newAffirmationModel of
                                        Just newAffirmationModel ->
                                            Affirmations.New.View.view newAffirmationModel
                                                |> Html.map translateNewAffirmationMsg

                                        Nothing ->
                                            text "This should not occur: newAffirmationModel == Nothing!"

                        ArgumentsRoute childRoute ->
                            case childRoute of
                                ArgumentRoute _ _ ->
                                    case model.argumentModel of
                                        Just argumentModel ->
                                            Arguments.Item.View.view argumentModel
                                                |> Html.map translateArgumentMsg

                                        Nothing ->
                                            text "This should not occur: argumentModel == Nothing!"

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

                        PropertiesRoute childRoute ->
                            case childRoute of
                                PropertyRoute _ _ ->
                                    case model.propertyModel of
                                        Just propertyModel ->
                                            Properties.Item.View.view propertyModel
                                                |> Html.map translatePropertyMsg

                                        Nothing ->
                                            text "This should not occur: propertyModel == Nothing!"

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
