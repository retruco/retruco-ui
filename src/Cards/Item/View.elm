module Cards.Item.View exposing (..)

import Cards.Item.Types exposing (..)
import Arguments.Index.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import LineViews exposing (viewCardLine)
import Properties.KeysAutocomplete.View
import Properties.SameObjectAndKey.View
import Statements.Toolbar.View
import Statements.ViewsHelpers
    exposing
        ( viewDebatePropertiesBlock
        , viewStatementPropertiesBlock
        , viewStatementRatingPanel
        )
import Urls
import Views


view : Model -> Html Msg
view model =
    case model.sameObjectAndKeyPropertiesModel of
        Just sameObjectAndKeyPropertiesModel ->
            Properties.SameObjectAndKey.View.view sameObjectAndKeyPropertiesModel
                |> Html.map translateSameObjectAndKeyPropertiesMsg

        Nothing ->
            let
                data =
                    model.data

                language =
                    model.language
            in
                case ( model.card, model.toolbarModel ) of
                    ( Just card, Just toolbarModel ) ->
                        div []
                            [ div [ class "align-items-center d-flex flex-nowrap justify-content-between mb-3" ]
                                [ h1 []
                                    [ viewCardLine
                                        language
                                        (Just (ForParent << Navigate))
                                        data
                                        card
                                    ]
                                , viewStatementRatingPanel language (ForParent << Navigate) False data card
                                ]
                            , Statements.Toolbar.View.view toolbarModel
                                |> Html.map translateToolbarMsg
                            , hr [] []
                            , ul [ class "nav nav-tabs" ]
                                [ li [ class "nav-item" ]
                                    [ aForPath
                                        (ForParent << Navigate)
                                        language
                                        (Urls.idToDebatePropertiesPath data card.id)
                                        [ classList
                                            [ ( "active"
                                              , case model.activeTab of
                                                    DebatePropertiesTab _ ->
                                                        True

                                                    _ ->
                                                        False
                                              )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Arguments ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath
                                        (ForParent << Navigate)
                                        language
                                        (Urls.idToPropertiesPath data card.id)
                                        [ classList
                                            [ ( "active", model.activeTab == PropertiesTab )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Properties ]
                                    ]
                                ]
                            , case model.activeTab of
                                DebatePropertiesTab argumentsModel ->
                                    Arguments.Index.View.view argumentsModel
                                        |> Html.map translateArgumentsMsg

                                PropertiesTab ->
                                    div []
                                        [ viewStatementPropertiesBlock language (ForParent << Navigate) data card
                                        , let
                                            controlId =
                                                "keysAutocomplete"
                                          in
                                            Properties.KeysAutocomplete.View.viewAutocomplete
                                                language
                                                controlId
                                                I18n.AddPropertyKey
                                                I18n.PropertyKeyPlaceholder
                                                Nothing
                                                model.keysAutocompleteModel
                                                |> Html.map translateKeysAutocompleteMsg
                                        ]
                            ]

                    ( _, _ ) ->
                        case model.httpError of
                            Just httpError ->
                                div
                                    [ class "alert alert-danger"
                                    , role "alert"
                                    ]
                                    [ strong []
                                        [ text <|
                                            I18n.translate language I18n.CardRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
