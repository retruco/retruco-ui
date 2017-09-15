module Statements.RatingPanels exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n
import Types exposing (DataProxy)


viewStatementIdRatingPanel : I18n.Language -> DataProxy a -> String -> Html msg
viewStatementIdRatingPanel language data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewStatementRatingPanel language data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewStatementRatingPanel language data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewStatementRatingPanel language data typedValue

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewStatementRatingPanel :
    I18n.Language
    -> DataProxy a
    -> { b | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingPanel language data { argumentCount, id, ratingCount, ratingSum, trashed } =
    div
        [ classList
            [ ( "bg-danger", trashed )
            , ( "border", True )
            , ( if trashed || ratingSum <= 0 then
                    "border-danger"
                else
                    "border-success"
              , True
              )
            , ( "lead", True )
            , ( "ml-3", True )
            , ( "p-2", True )
            , ( if trashed then
                    "text-white"
                else if ratingSum > 0 then
                    "text-success"
                else
                    "text-danger"
              , True
              )
            ]
        ]
        [ div [ class "text-center text-nowrap" ]
            [ strong [] [ text <| toString ratingSum ]
            , text " / "
            , text <|
                I18n.translate
                    language
                    (I18n.CountVotes ratingCount)
            ]
        , div [ class "text-center text-nowrap" ]
            [ text <|
                I18n.translate
                    language
                    (I18n.CountArguments argumentCount)
            ]
        ]
