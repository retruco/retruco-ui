module Statements.Ratings exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import I18n
import Types exposing (DataProxy, Statement)


idToStatement : DataProxy a -> String -> Maybe (Statement {})
idToStatement data id =
    case Dict.get id data.cards of
        Just card ->
            Just
                { argumentCount = card.argumentCount
                , ballotId = card.ballotId
                , createdAt = card.createdAt
                , id = card.id
                , qualities = card.qualities
                , ratingCount = card.ratingCount
                , ratingSum = card.ratingSum
                , trashed = card.trashed
                , type_ = card.type_
                }

        Nothing ->
            case Dict.get id data.properties of
                Just property ->
                    Just
                        { argumentCount = property.argumentCount
                        , ballotId = property.ballotId
                        , createdAt = property.createdAt
                        , id = property.id
                        , qualities = property.qualities
                        , ratingCount = property.ratingCount
                        , ratingSum = property.ratingSum
                        , trashed = property.trashed
                        , type_ = property.type_
                        }

                Nothing ->
                    case Dict.get id data.values of
                        Just typedValue ->
                            Just
                                { argumentCount = typedValue.argumentCount
                                , ballotId = typedValue.ballotId
                                , createdAt = typedValue.createdAt
                                , id = typedValue.id
                                , qualities = typedValue.qualities
                                , ratingCount = typedValue.ratingCount
                                , ratingSum = typedValue.ratingSum
                                , trashed = typedValue.trashed
                                , type_ = typedValue.type_
                                }

                        Nothing ->
                            Nothing


viewStatementIdRatingBadges : I18n.Language -> DataProxy a -> String -> Html msg
viewStatementIdRatingBadges language data id =
    case idToStatement data id of
        Just statement ->
            viewStatementRatingBadges language data statement

        Nothing ->
            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId id) ]


viewStatementRatingBadges :
    I18n.Language
    -> DataProxy a
    -> { b | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingBadges language data { argumentCount, id, ratingCount, ratingSum, trashed } =
    span [ class "d-flex align-items-baseline flex-nowrap ml-2" ]
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
                        [ title
                            (toString ratingSum
                                ++ " / "
                                ++ I18n.translate language (I18n.CountVotes ratingCount)
                            )
                        ]
                        [ text <| toString ratingSum
                        , text " "
                        , span
                            [ ariaHidden True
                            , classList
                                [ ( "fa", True )
                                , ( if ratingSum > 0 then
                                        "fa-arrow-up"
                                    else if ratingSum == 0 then
                                        "fa-arrow-right"
                                    else
                                        "fa-arrow-down"
                                  , True
                                  )
                                ]
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
