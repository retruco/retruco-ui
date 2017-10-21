module DebateProperties.SameObject.View exposing (..)

import Array
import DebateProperties.New.View
import DebateProperties.SameObject.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
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
        case model.debatePropertyIds of
            Just debatePropertyIds ->
                div []
                    [ div []
                        [ if Array.isEmpty debatePropertyIds then
                            p [] [ text <| I18n.translate language I18n.MissingArguments ]
                          else
                            div [ class "list-group" ]
                                (Array.toList debatePropertyIds
                                    |> List.map
                                        (\debatePropertyId ->
                                            let
                                                classList =
                                                    case Dict.get debatePropertyId data.properties of
                                                        Just debateProperty ->
                                                            case debateProperty.keyId of
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
                                                    debatePropertyId
                                        )
                                )
                        ]
                    , hr [] []
                    , DebateProperties.New.View.view model.newDebatePropertyModel
                        |> Html.map translateNewDebatePropertyMsg
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
