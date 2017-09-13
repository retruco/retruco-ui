module DebateProperties.SameObject.View exposing (..)

import Array
import DebateProperties.New.View
import DebateProperties.SameObject.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Statements.Lines exposing (viewPropertyIdLine)
import Statements.RatingPanels exposing (viewStatementIdRatingPanel)
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
                            ul [ class "list-group" ]
                                (Array.toList debatePropertyIds
                                    |> List.map
                                        (\debatePropertyId ->
                                            li [ class "align-items-center d-flex flex-nowrap justify-content-between list-group-item" ]
                                                [ viewPropertyIdLine language
                                                    (Just navigateMsg)
                                                    False
                                                    data
                                                    debatePropertyId
                                                , viewStatementIdRatingPanel
                                                    language
                                                    (Just navigateMsg)
                                                    data
                                                    debatePropertyId
                                                ]
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
