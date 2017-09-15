module Properties.Item.View exposing (..)

import DebateProperties.SameObject.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Properties.Item.Types exposing (..)
import Properties.SameObject.View
import Properties.SameObjectAndKey.View
import Properties.SameValue.View
import Statements.Lines exposing (lineIdAttributes, viewPropertyIdLine, viewStatementIdLine)
import Statements.RatingPanels exposing (viewStatementIdRatingPanel, viewStatementRatingPanel)
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

                navigateMsg =
                    ForParent << Navigate
            in
                case ( model.property, model.toolbarModel ) of
                    ( Just property, Just toolbarModel ) ->
                        div []
                            [ div
                                (lineIdAttributes language Nothing [] data property.id)
                                [ div [ class "mb-3 w-100" ]
                                    [ div
                                        (lineIdAttributes
                                            language
                                            (Just navigateMsg)
                                            [ ( "ml-4", True ) ]
                                            data
                                            property.objectId
                                        )
                                        [ div [ class "lead" ]
                                            [ viewStatementIdLine
                                                language
                                                True
                                                False
                                                data
                                                property.objectId
                                            ]
                                        , viewStatementIdRatingPanel language data property.objectId
                                        ]
                                    , let
                                        keyLabel =
                                            Dict.get property.keyId I18n.keyLabelById
                                                |> Maybe.map (I18n.translate language)
                                                |> Maybe.withDefault property.keyId
                                      in
                                        div [ class "align-items-baseline d-flex flex-nowrap" ]
                                            [ span
                                                [ ariaHidden True
                                                , classList
                                                    [ ( "fa", True )
                                                    , ( if property.keyId == "con" then
                                                            "fa-minus"
                                                        else if property.keyId == "pro" then
                                                            "fa-plus"
                                                        else
                                                            "fa-circle"
                                                      , True
                                                      )
                                                    , ( "fa-fw", True )
                                                    , ( "mr-2", True )
                                                    ]
                                                ]
                                                []
                                            , span [ class "lead" ] [ text keyLabel ]
                                            ]
                                    , div
                                        (lineIdAttributes
                                            language
                                            (Just navigateMsg)
                                            [ ( "ml-4", True ) ]
                                            data
                                            property.valueId
                                        )
                                        [ div [ class "lead" ]
                                            [ viewStatementIdLine
                                                language
                                                True
                                                False
                                                data
                                                property.valueId
                                            ]
                                        , viewStatementIdRatingPanel language data property.valueId
                                        ]
                                    ]
                                , viewStatementRatingPanel language data property
                                ]
                            , case model.similarDebatePropertyIds of
                                Just similarDebatePropertyIds ->
                                    let
                                        similarDebatePropertyCount =
                                            List.length similarDebatePropertyIds
                                    in
                                        if similarDebatePropertyCount > 0 then
                                            div [ class "alert alert-warning", role "alert" ]
                                                [ h3 [ class "alert-heading" ]
                                                    [ text <|
                                                        I18n.translate language <|
                                                            I18n.SimilarArgumentsTitle
                                                                similarDebatePropertyCount
                                                    ]
                                                , p []
                                                    [ text <|
                                                        I18n.translate language <|
                                                            I18n.SimilarArgumentsDescription
                                                                similarDebatePropertyCount
                                                    ]
                                                , ul [ class "list-group" ]
                                                    (List.map
                                                        (\similarDebatePropertyId ->
                                                            li
                                                                (lineIdAttributes
                                                                    language
                                                                    (Just navigateMsg)
                                                                    [ ( "list-group-item", True ) ]
                                                                    data
                                                                    similarDebatePropertyId
                                                                )
                                                                [ viewPropertyIdLine
                                                                    language
                                                                    True
                                                                    data
                                                                    similarDebatePropertyId
                                                                , viewStatementIdRatingPanel
                                                                    language
                                                                    data
                                                                    similarDebatePropertyId
                                                                ]
                                                        )
                                                        similarDebatePropertyIds
                                                    )
                                                ]
                                        else
                                            text ""

                                Nothing ->
                                    text ""
                            , Statements.Toolbar.View.view toolbarModel
                                |> Html.map translateToolbarMsg
                            , hr [] []
                            , ul [ class "nav nav-tabs" ]
                                [ li [ class "nav-item" ]
                                    [ aForPath
                                        navigateMsg
                                        language
                                        (Urls.idToDebatePropertiesPath data property.id)
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
                                        language
                                        (Urls.idToPropertiesPath data property.id)
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
                                        language
                                        (Urls.idToPropertiesAsValuePath data property.id)
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
                                            I18n.translate language I18n.ArgumentsRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
