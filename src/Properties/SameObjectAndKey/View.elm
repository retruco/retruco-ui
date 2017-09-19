module Properties.SameObjectAndKey.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Properties.SameObjectAndKey.Types exposing (..)
import Statements.Lines exposing (viewStatementIdLine, viewStatementIdRatedLine)
import Statements.Ratings exposing (viewStatementIdRatingBadges)
import Strings
import Urls
import Values.New.View
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language

        navigateMsg =
            ForParent << Navigate
    in
        case model.propertyIds of
            Just propertyIds ->
                div []
                    [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                        [ div [ class "mb-3 w-100" ]
                            [ viewStatementIdRatedLine
                                div
                                language
                                True
                                navigateMsg
                                [ ( "lead", True ), ( "ml-4", True ) ]
                                True
                                data
                                model.objectId
                            , let
                                keyLabel =
                                    Dict.get model.keyId I18n.keyLabelById
                                        |> Maybe.map (I18n.translate language)
                                        |> Maybe.withDefault (Strings.idToString language data model.keyId)
                              in
                                div [ class "align-items-baseline d-flex flex-nowrap" ]
                                    [ span
                                        [ ariaHidden True
                                        , classList
                                            [ ( "fa", True )
                                            , ( if model.keyId == "con" then
                                                    "fa-minus"
                                                else if model.keyId == "pro" then
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
                            ]
                        ]
                    , hr [] []
                    , div [ class "list-group" ]
                        (Array.toList propertyIds
                            |> List.filterMap
                                (\propertyId ->
                                    case Dict.get propertyId data.properties of
                                        Just property ->
                                            Just <|
                                                aForPath
                                                    navigateMsg
                                                    language
                                                    (Urls.idToPath data property.id)
                                                    [ classList
                                                        [ ( "align", True )
                                                        , ( "align-items-top", True )
                                                        , ( "d-flex", True )
                                                        , ( "flex-nowrap", True )
                                                        , ( "justify-content-between", True )
                                                        , ( "lead", True )
                                                        , ( "list-group-item", True )
                                                        , ( "list-group-item-action", True )
                                                        ]
                                                    ]
                                                    [ viewStatementIdLine
                                                        language
                                                        navigateMsg
                                                        True
                                                        False
                                                        data
                                                        property.valueId
                                                    , viewStatementIdRatingBadges language data property.id
                                                    ]

                                        Nothing ->
                                            Nothing
                                )
                        )
                    , hr [] []
                    , Values.New.View.view model.newValueModel
                        |> Html.map translateNewValueMsg
                    ]

            Nothing ->
                case model.httpError of
                    Just httpError ->
                        div
                            [ class "alert alert-danger"
                            , role "alert"
                            ]
                            [ strong []
                                [ text <|
                                    I18n.translate language I18n.SameObjectAndKeyPropertiesRetrievalFailed
                                        ++ I18n.translate language I18n.Colon
                                ]
                            , text <| Http.Error.toString language httpError
                            ]

                    Nothing ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]
