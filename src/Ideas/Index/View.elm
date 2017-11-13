module Ideas.Index.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Ideas.Index.Types exposing (..)
import Ideas.New.View
import Statements.Lines exposing (viewStatementIdRatedListGroupLine)
import Views


view : Model -> Html Msg
view model =
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
        case model.discussionPropertyIds of
            Just discussionPropertyIds ->
                div []
                    [ div []
                        [ if Array.isEmpty discussionPropertyIds then
                            p [] [ text <| I18n.translate language I18n.MissingArguments ]
                          else
                            div [ class "list-group" ]
                                (Array.toList discussionPropertyIds
                                    |> List.map
                                        (\discussionPropertyId ->
                                            let
                                                classList =
                                                    case Dict.get discussionPropertyId data.properties of
                                                        Just discussionProperty ->
                                                            case discussionProperty.keyId of
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
                                                    embed
                                                    language
                                                    navigateMsg
                                                    ""
                                                    classList
                                                    False
                                                    data
                                                    discussionPropertyId
                                        )
                                )
                        ]
                    , hr [] []
                    , Ideas.New.View.view model.newIdeaModel
                        |> Html.map translateNewIdeaMsg
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
