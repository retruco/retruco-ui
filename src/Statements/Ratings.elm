module Statements.Ratings exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import I18n
import Types exposing (DataProxy)


viewStatementIdRatingBadges : I18n.Language -> DataProxy a -> String -> Html msg
viewStatementIdRatingBadges language data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewStatementRatingBadges language data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewStatementRatingBadges language data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewStatementRatingBadges language data typedValue

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewStatementRatingBadges :
    I18n.Language
    -> DataProxy a
    -> { b | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingBadges language data { argumentCount, id, ratingCount, ratingSum, trashed } =
    span [ class "ml-2" ]
        ((if trashed then
            [ span
                [ class "badge badge-danger badge-pill mr-1"
                , title (I18n.translate language I18n.Trash)
                ]
                [ span
                    [ ariaHidden True
                    , class "fa fa-trash"
                    ]
                    []
                ]
            ]
          else
            []
         )
            ++ [ span
                    [ classList
                        [ ( "badge", True )
                        , ( "badge-pill", True )
                        , ( if ratingSum > 0 then
                                "badge-success"
                            else
                                "badge-warning"
                          , True
                          )
                        ]
                    ]
                    [ span
                        [ title (toString ratingSum ++ " / " ++ I18n.translate language (I18n.CountVotes ratingCount))
                        ]
                        [ text <| toString ratingSum
                        , text " "
                        , span
                            [ ariaHidden True
                            , classList
                                [ ( "fa", True )
                                , ( if ratingSum > 0 then
                                        "fa-arrow-up"
                                    else
                                        "fa-arrow-down"
                                  , True
                                  )
                                ]
                            , title (toString ratingSum ++ " / " ++ I18n.translate language (I18n.CountVotes ratingCount))
                            ]
                            []
                        ]
                    , span
                        [ class "ml-2"
                        , title (I18n.translate language (I18n.CountArguments argumentCount))
                        ]
                        [ text <| toString argumentCount
                        , text " "
                        , span
                            [ ariaHidden True
                            , class "fa fa-comment"
                            ]
                            []
                        ]
                    ]
               ]
        )
