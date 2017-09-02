module Values.Item.View exposing (..)

import Arguments.Index.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import LineViews exposing (viewValueTypeLine)
import SameKeyProperties.View
import Statements.Toolbar.View
import Statements.ViewsHelpers
    exposing
        ( viewDebatePropertiesBlock
        , viewStatementPropertiesBlock
        , viewStatementRatingPanel
        )
import Urls
import Values.Item.Types exposing (..)
import Views


view : Model -> Html Msg
view model =
    case model.sameKeyPropertiesModel of
        Just sameKeyPropertiesModel ->
            SameKeyProperties.View.view sameKeyPropertiesModel
                |> Html.map translateSameKeyPropertiesMsg

        Nothing ->
            let
                data =
                    model.data

                language =
                    model.language
            in
                case ( model.typedValue, model.toolbarModel ) of
                    ( Just typedValue, Just toolbarModel ) ->
                        div []
                            [ div [ class "align-items-center d-flex flex-nowrap justify-content-between mb-3" ]
                                [ h1 []
                                    [ viewValueTypeLine
                                        language
                                        (Just (ForParent << Navigate))
                                        False
                                        data
                                        typedValue.value
                                    ]
                                , viewStatementRatingPanel language (ForParent << Navigate) False data typedValue
                                ]
                            , Statements.Toolbar.View.view toolbarModel
                                |> Html.map translateToolbarMsg
                            , hr [] []
                            , ul [ class "nav nav-tabs" ]
                                [ li [ class "nav-item" ]
                                    [ aForPath
                                        (ForParent << Navigate)
                                        language
                                        (Urls.idToDebatePropertiesPath data typedValue.id)
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
                                        (Urls.idToPropertiesPath data typedValue.id)
                                        [ classList
                                            [ ( "active", model.activeTab == PropertiesTab )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Properties ]
                                    ]
                                , li [ class "nav-item" ]
                                    [ aForPath
                                        (ForParent << Navigate)
                                        language
                                        ((Urls.idToPath data typedValue.id) ++ "/details")
                                        [ classList
                                            [ ( "active", model.activeTab == DetailsTab )
                                            , ( "nav-link", True )
                                            ]
                                        ]
                                        [ text <| I18n.translate language I18n.Details ]
                                    ]
                                ]
                            , case model.activeTab of
                                DebatePropertiesTab argumentsModel ->
                                    Arguments.Index.View.view argumentsModel
                                        |> Html.map translateArgumentsMsg

                                DetailsTab ->
                                    viewValueTypeLine
                                        language
                                        (Just (ForParent << Navigate))
                                        True
                                        data
                                        typedValue.value

                                PropertiesTab ->
                                    viewStatementPropertiesBlock language (ForParent << Navigate) data typedValue
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
                                            I18n.translate language I18n.ValueRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
