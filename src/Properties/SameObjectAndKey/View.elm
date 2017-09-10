module Properties.SameObjectAndKey.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import LineViews exposing (viewStatementIdLine)
import Properties.SameObjectAndKey.Types exposing (..)
import Statements.RatingPanels exposing (viewStatementIdRatingPanel, viewStatementRatingPanel)
import Strings
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
                            [ div [ class "align-items-center d-flex flex-nowrap justify-content-between ml-4" ]
                                [ div [ class "lead" ]
                                    [ viewStatementIdLine
                                        language
                                        (Just navigateMsg)
                                        True
                                        False
                                        data
                                        model.objectId
                                    ]
                                , viewStatementIdRatingPanel
                                    language
                                    (Just navigateMsg)
                                    data
                                    model.objectId
                                ]
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
                    , ul [ class "list-group" ]
                        (Array.toList propertyIds
                            |> List.filterMap
                                (\propertyId ->
                                    case Dict.get propertyId data.properties of
                                        Just property ->
                                            Just <|
                                                li [ class "align-items-center d-flex flex-nowrap justify-content-between list-group-item" ]
                                                    [ viewStatementIdLine language Nothing True False data property.valueId
                                                    , viewStatementRatingPanel
                                                        language
                                                        (Just navigateMsg)
                                                        data
                                                        property
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
