module Situations.Item.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Situations.Item.Types exposing (..)
import Situations.NewSuggestion.View
import Statements.Lines exposing (viewStatementIdRatedListGroupLine)
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
        case model.situationPropertyIds of
            Just situationPropertyIds ->
                div []
                    [ div []
                        [ if Array.isEmpty situationPropertyIds then
                            p [] [ text <| I18n.translate language I18n.MissingArguments ]
                          else
                            div [ class "list-group" ]
                                (Array.toList situationPropertyIds
                                    |> List.map
                                        (\situationPropertyId ->
                                            let
                                                classList =
                                                    case Dict.get situationPropertyId data.properties of
                                                        Just situationProperty ->
                                                            case situationProperty.keyId of
                                                                "con" ->
                                                                    [ ( "list-group-item-warning", True ) ]

                                                                "pro" ->
                                                                    [ ( "list-group-item-success", True ) ]

                                                                _ ->
                                                                    [ ( "list-group-item-secondary", True ) ]

                                                        Nothing ->
                                                            []
                                            in
                                                viewStatementIdRatedListGroupLine
                                                    language
                                                    navigateMsg
                                                    ""
                                                    classList
                                                    False
                                                    data
                                                    situationPropertyId
                                        )
                                )
                        ]
                    , hr [] []
                    , Situations.NewSuggestion.View.view model.newSuggestionModel
                        |> Html.map translateNewSuggestionMsg
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
                                    I18n.translate language I18n.ArgumentsRetrievalFailed
                                        ++ I18n.translate language I18n.Colon
                                ]
                            , text <| Http.Error.toString language httpError
                            ]

                    Nothing ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]
