module Questions.Index.View exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n
import Questions.Index.Types exposing (..)
import Interventions.New.View
import Statements.Lines exposing (viewStatementIdRatedListGroupLine)


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
                [ if Array.isEmpty model.questionProperties then
                    p [] [ text <| I18n.translate language I18n.MissingQuestions ]
                  else
                    div [ class "list-group" ]
                        (Array.toList model.questionProperties
                            |> List.map
                                (\questionProperty ->
                                    viewStatementIdRatedListGroupLine
                                        embed
                                        language
                                        navigateMsg
                                        ""
                                        []
                                        False
                                        data
                                        questionProperty.id
                                )
                        )
                ]
            , hr [] []
            , Interventions.New.View.view model.newInterventionModel
                |> Html.map translateNewInterventionMsg
            ]
