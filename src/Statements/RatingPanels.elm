module Statements.RatingPanels exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Types exposing (DataProxy)
import Urls


viewStatementIdRatingPanel : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> String -> Html msg
viewStatementIdRatingPanel language navigateMsg data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewStatementRatingPanel language navigateMsg data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewStatementRatingPanel language navigateMsg data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewStatementRatingPanel language navigateMsg data typedValue

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewStatementRatingPanel :
    I18n.Language
    -> Maybe (String -> msg)
    -> DataProxy a
    -> { b | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingPanel language navigateMsg data { argumentCount, id, ratingCount, ratingSum, trashed } =
    let
        buttonClass =
            classList
                [ ( "btn", True )
                , ( "btn-lg", True )
                , ( if trashed then
                        "btn-danger"
                    else if ratingSum > 0 then
                        "btn-outline-success"
                    else
                        "btn-outline-danger"
                  , True
                  )
                , ( "ml-3", True )
                ]

        buttonWithAttributes =
            case navigateMsg of
                Just navigateMsg ->
                    aForPath
                        navigateMsg
                        language
                        (Urls.idToPath data id)
                        [ buttonClass ]

                Nothing ->
                    button
                        [ buttonClass
                        , disabled True
                        , type_ "button"
                        ]
    in
        buttonWithAttributes
            [ strong [] [ text <| toString ratingSum ]
            , text " / "
            , text <|
                I18n.translate
                    language
                    (I18n.CountVotes ratingCount)
            , br [] []
            , text <|
                I18n.translate
                    language
                    (I18n.CountArguments argumentCount)
            ]
