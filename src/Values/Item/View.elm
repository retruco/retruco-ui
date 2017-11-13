module Values.Item.View exposing (..)

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
import Statements.Lines exposing (viewStatementIdRatedLine, viewValueTypeLine)
import Statements.Toolbar.View
import Urls
import Values.Item.Types exposing (..)
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

                embed =
                    model.embed

                language =
                    model.language

                navigateMsg =
                    ForParent << Navigate
            in
                case ( model.typedValue, model.toolbarModel ) of
                    ( Just typedValue, Just toolbarModel ) ->
                        div []
                            [ viewStatementIdRatedLine
                                h1
                                embed
                                language
                                False
                                navigateMsg
                                [ ( "h4", True ), ( "mb-3", True ) ]
                                True
                                data
                                typedValue.id
                            , viewDuplicatedByAlert
                                embed
                                language
                                navigateMsg
                                data
                                model.duplicatedByPropertyIds
                            , viewDuplicateOfAlert
                                embed
                                language
                                navigateMsg
                                data
                                model.duplicateOfPropertyIds
                            , Statements.Toolbar.View.view toolbarModel
                                |> Html.map translateToolbarMsg
                            , hr [] []
                            , ul [ class "nav nav-tabs" ]
                                [ li [ class "nav-item" ]
                                    [ aForPath
                                        navigateMsg
                                        embed
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
                                        navigateMsg
                                        embed
                                        language
                                        (Urls.idToPropertiesPath data typedValue.id)
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
                                        navigateMsg
                                        embed
                                        language
                                        (Urls.idToPropertiesAsValuePath data typedValue.id)
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
                                , li [ class "nav-item" ]
                                    [ aForPath
                                        navigateMsg
                                        embed
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
                                DebatePropertiesTab debatePropertiesModel ->
                                    DebateProperties.SameObject.View.view debatePropertiesModel
                                        |> Html.map translateDebatePropertiesMsg

                                DetailsTab ->
                                    viewValueTypeLine
                                        embed
                                        language
                                        navigateMsg
                                        True
                                        data
                                        typedValue.value

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
                                            I18n.translate language I18n.ValueRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
