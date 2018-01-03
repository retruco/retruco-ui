module Interventions.Index.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Interventions.Index.Types exposing (..)
import Interventions.New.View
import Statements.Lines exposing (viewStatementIdLine, viewStatementIdRatedListGroupLine)
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
        div []
            [ div []
                [ if Array.isEmpty model.interventionProperties then
                    p [] [ text <| I18n.translate language I18n.MissingInterventions ]
                  else
                    div [ class "list-group" ]
                        (Array.toList model.interventionProperties
                            |> List.map
                                (\interventionProperty ->
                                    case Dict.get interventionProperty.valueId model.ideaPropertyByValueId of
                                        Just ideaProperty ->
                                            viewStatementIdRatedListGroupLine
                                                embed
                                                language
                                                navigateMsg
                                                ""
                                                []
                                                False
                                                data
                                                ideaProperty.id

                                        Nothing ->
                                            case
                                                Dict.get interventionProperty.valueId model.questionPropertyByValueId
                                            of
                                                Just questionProperty ->
                                                    viewStatementIdRatedListGroupLine
                                                        embed
                                                        language
                                                        navigateMsg
                                                        ""
                                                        []
                                                        False
                                                        data
                                                        questionProperty.id

                                                Nothing ->
                                                    viewInterventionListGroupLine
                                                        embed
                                                        language
                                                        navigateMsg
                                                        ""
                                                        data
                                                        interventionProperty
                                )
                        )
                ]
            , hr [] []
            , Interventions.New.View.view model.newInterventionModel
                |> Html.map translateNewInterventionMsg
            ]


viewInterventionListGroupLine :
    Bool
    -> I18n.Language
    -> (String -> msg)
    -> String
    -> DataProxy a
    -> Property
    -> Html msg
viewInterventionListGroupLine embed language navigateMsg path data interventionProperty =
    aForPath
        navigateMsg
        embed
        language
        (Urls.idToPath data interventionProperty.id ++ path)
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
        [ viewStatementIdLine
            embed
            language
            navigateMsg
            True
            False
            data
            interventionProperty.valueId
        ]
