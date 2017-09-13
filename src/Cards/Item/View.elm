module Cards.Item.View exposing (..)

import Cards.Item.Types exposing (..)
import DebateProperties.SameObject.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Properties.SameObject.View
import Properties.SameObjectAndKey.View
import Properties.SameValue.View
import Statements.Alerts exposing (viewDuplicatedByAlert, viewDuplicateOfAlert)
import Statements.Lines exposing (viewCardLine)
import Statements.RatingPanels exposing (viewStatementRatingPanel)
import Statements.Toolbar.View
import Urls
import Views


view : Model -> Html Msg
view model =
    case model.sameKeyPropertiesModel of
        Just sameKeyPropertiesModel ->
            Properties.SameObjectAndKey.View.view sameKeyPropertiesModel
                |> Html.map translateSameKeyPropertiesMsg

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
                                , viewStatementRatingPanel language Nothing data card
                                ]
                            , viewDuplicatedByAlert
                                language
                                (ForParent << Navigate)
                                data
                                model.duplicatedByPropertyIds
                            , viewDuplicateOfAlert
                                language
                                (ForParent << Navigate)
                                data
                                model.duplicateOfPropertyIds
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
                                            [ ( "active"
                                              , case model.activeTab of
                                                    PropertiesTab _ ->
                                                        True

                                                    _ ->
                                                        False
                                              )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Properties ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath
                                        (ForParent << Navigate)
                                        language
                                        (Urls.idToPropertiesAsValuePath data card.id)
                                        [ classList
                                            [ ( "active"
                                              , case model.activeTab of
                                                    PropertiesAsValueTab _ ->
                                                        True

                                                    _ ->
                                                        False
                                              )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Uses ]
                                    ]
                                ]
                            , case model.activeTab of
                                DebatePropertiesTab debatePropertiesModel ->
                                    DebateProperties.SameObject.View.view debatePropertiesModel
                                        |> Html.map translateDebatePropertiesMsg

                                NoTab ->
                                    text ""

                                PropertiesAsValueTab propertiesAsValueModel ->
                                    Properties.SameValue.View.view propertiesAsValueModel
                                        |> Html.map translatePropertiesAsValueMsg

                                PropertiesTab propertiesModel ->
                                    Properties.SameObject.View.view propertiesModel
                                        |> Html.map translatePropertiesMsg
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
