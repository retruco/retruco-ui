module Root.View exposing (..)

import About.View
import Authenticator.View
import Cards.Index.View
import Cards.Item.View
import Cards.New.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Html.Helpers exposing (aForPath)
import I18n
import Json.Decode
import Properties.Item.View
import Proposals.Index.View
import Proposals.New.View
import Root.Types exposing (..)
import Routes exposing (..)
import Situations.Index.View
import Situations.New.View
import Situations.Routes exposing (..)
import Urls
import Values.Index.View
import Values.Item.View
import Values.New.View
import Views exposing (..)


view : Model -> Html Msg
view model =
    case model.route of
        (I18nRouteWithLanguage language localizedRoute) as route ->
            let
                languageNavItem =
                    li [ class "dropdown nav-item" ]
                        [ a
                            [ attribute "aria-expanded" "false"
                            , attribute "aria-haspopup" "true"
                            , class "dropdown-toggle nav-link"
                            , attribute "data-toggle" "dropdown"
                            , href ""
                            , id "language-dropdown-menu"
                            ]
                            [ text (I18n.translate language (I18n.Language language))
                            ]
                        , div [ attribute "aria-labelledby" "language-dropdown-menu", class "dropdown-menu" ]
                            (I18n.languages
                                |> List.map
                                    (\language ->
                                        ( language
                                        , (I18n.translate language (I18n.Language language))
                                        )
                                    )
                                |> List.sortBy (\( language, languageLabel ) -> languageLabel)
                                |> List.map
                                    (\( otherLanguage, otherLanguageLabel ) ->
                                        let
                                            path =
                                                Urls.replaceLanguageInLocation otherLanguage model.location
                                        in
                                            a
                                                [ classList
                                                    [ ( "dropdown-item", True )
                                                    , ( "disabled", otherLanguage == language )
                                                    ]
                                                , href path
                                                , onWithOptions
                                                    "click"
                                                    { stopPropagation = False, preventDefault = True }
                                                    (Json.Decode.succeed (Navigate path))
                                                ]
                                                [ text otherLanguageLabel ]
                                    )
                            )
                        ]

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
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/cards"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Cards ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/situations"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Situations ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath Navigate
                                        language
                                        "/proposals"
                                        [ class "nav-link" ]
                                        [ text <| I18n.translate language I18n.Proposals ]
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
                                [ languageNavItem
                                , profileNavItem
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

                                NewCardRoute ->
                                    case model.newCardModel of
                                        Just newCardModel ->
                                            Cards.New.View.view newCardModel
                                                |> Html.map translateNewCardMsg

                                        Nothing ->
                                            text "This should not occur: newCardModel == Nothing!"

                        HomeRoute ->
                            text "Home page is currently a redirection."

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

                        ProposalsRoute childRoute ->
                            case childRoute of
                                ProposalsIndexRoute ->
                                    case model.proposalsModel of
                                        Just proposalsModel ->
                                            Proposals.Index.View.view proposalsModel
                                                |> Html.map translateProposalsMsg

                                        Nothing ->
                                            text "This should not occur: proposalsModel == Nothing!"

                                NewProposalRoute ->
                                    case model.newProposalModel of
                                        Just newProposalModel ->
                                            Proposals.New.View.view newProposalModel
                                                |> Html.map translateNewProposalMsg

                                        Nothing ->
                                            text "This should not occur: newProposalModel == Nothing!"

                        SituationsRoute childRoute ->
                            case childRoute of
                                SituationsIndexRoute ->
                                    case model.situationsModel of
                                        Just situationsModel ->
                                            Situations.Index.View.view situationsModel
                                                |> Html.map translateSituationsMsg

                                        Nothing ->
                                            text "This should not occur: situationsModel == Nothing!"

                                NewSituationRoute ->
                                    case model.newSituationModel of
                                        Just newSituationModel ->
                                            Situations.New.View.view newSituationModel
                                                |> Html.map translateNewSituationMsg

                                        Nothing ->
                                            text "This should not occur: newSituationModel == Nothing!"

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
                    , p [] [ text " " ]
                    , footer [ class "footer row w-100" ]
                        [ div [ class "bg-secondary pb-2 pt-2 text-center text-white  w-100" ]
                            [ text <| I18n.translate language I18n.Copyright
                            , text " — "
                            , text <| I18n.translate language I18n.RetrucoIsFreeSoftware
                            , text " "
                            , a
                                [ class "btn btn-light btn-sm"
                                , href "https://framagit.org/retruco/retruco-ui"
                                , target "_blank"
                                ]
                                [ text <| I18n.translate language I18n.Contribute ]
                            ]
                        ]
                    ]

        I18nRouteWithoutLanguage _ ->
            div [ style [ ( "min-height", "60em" ) ] ]
                [ viewBigMessage "" "" ]



-- StatementsRoute nestedRoute ->
--     Html.map translateStatementsMsg (Statements.view model.authentication model.statementsModel)
