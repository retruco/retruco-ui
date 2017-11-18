module Interventions.Index.View exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Interventions.Index.Types exposing (..)
import Interventions.New.View
import Statements.Lines exposing (viewStatementIdLine)
import Types exposing (DataProxy, Property)
import Urls


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
        case model.discussionProperties of
            Just discussionProperties ->
                div []
                    [ div []
                        [ if Array.isEmpty discussionProperties then
                            p [] [ text <| I18n.translate language I18n.MissingInterventions ]
                          else
                            div [ class "list-group" ]
                                (Array.toList discussionProperties
                                    |> List.map
                                        (\discussionProperty ->
                                            viewInterventionListGroupLine
                                                embed
                                                language
                                                navigateMsg
                                                ""
                                                data
                                                discussionProperty
                                        )
                                )
                        ]
                    , hr [] []
                    , Interventions.New.View.view model.newInterventionModel
                        |> Html.map translateNewInterventionMsg
                    ]

            Nothing ->
                text ""


viewInterventionListGroupLine :
    Bool
    -> I18n.Language
    -> (String -> msg)
    -> String
    -> DataProxy a
    -> Property
    -> Html msg
viewInterventionListGroupLine embed language navigateMsg path data discussionProperty =
    aForPath
        navigateMsg
        embed
        language
        (Urls.idToPath data discussionProperty.id ++ path)
        [ classList
            [ ( "align", True )
            , ( "align-items-top", True )
            , ( "d-flex", True )
            , ( "flex-nowrap", True )
            , ( "justify-content-between", True )
            , ( "lead", True )
            , ( "list-group-item", True )
            , ( "list-group-item-action", True )

            -- , ( "list-group-item-secondary", True )
            ]
        ]
        [ div
            [ classList [ ( "bg-white", True ), ( "ml-4", True ) ] ]
            [ aForPath
                navigateMsg
                embed
                language
                (Urls.idToPath data discussionProperty.valueId)
                [ classList
                    [ ( "align", True )
                    , ( "align-items-top", True )
                    , ( "d-flex", True )
                    , ( "flex-nowrap", True )
                    , ( "justify-content-between", True )
                    , ( "text-dark", True )
                    ]
                ]
                [ viewStatementIdLine
                    embed
                    language
                    navigateMsg
                    True
                    False
                    data
                    discussionProperty.valueId
                ]
            ]
        ]
