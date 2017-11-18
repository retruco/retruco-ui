module Ideas.Index.View exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n
import Ideas.Index.Types exposing (..)
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
                [ if Array.isEmpty model.ideaProperties then
                    p [] [ text <| I18n.translate language I18n.MissingIdeas ]
                  else
                    div [ class "list-group" ]
                        (Array.toList model.ideaProperties
                            |> List.map
                                (\ideaProperty ->
                                    viewStatementIdRatedListGroupLine
                                        embed
                                        language
                                        navigateMsg
                                        ""
                                        []
                                        False
                                        data
                                        ideaProperty.id
                                )
                        )
                ]
            , hr [] []
            , Interventions.New.View.view model.newInterventionModel
                |> Html.map translateNewInterventionMsg
            ]
