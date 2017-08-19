module Statements.ViewsHelpers exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick)
import Html.Helpers exposing (aForPath)
import I18n
import LineViews exposing (viewPropertyIdLine)
import Set exposing (Set)
import Types exposing (Argument, DataProxy)


debatePropertyKeyIds : Set String
debatePropertyKeyIds =
    Set.fromList [ "cons", "pro" ]


viewDebatePropertiesBlock :
    I18n.Language
    -> (String -> msg)
    -> DataProxy a
    -> List String
    -> Html msg
viewDebatePropertiesBlock language navigateMsg data debatePropertyIds =
    div []
        [ h2 [] [ text <| I18n.translate language I18n.Arguments ]
        , if List.isEmpty debatePropertyIds then
            p [] [ text <| I18n.translate language I18n.MissingArguments ]
          else
            ul [ class "list-group" ]
                (List.map
                    (\debatePropertyId ->
                        li [ class "d-flex flex-nowrap justify-content-between list-group-item" ]
                            [ viewPropertyIdLine language (Just navigateMsg) False data debatePropertyId
                            , viewStatementIdRatingPanel language navigateMsg data debatePropertyId
                            ]
                    )
                    debatePropertyIds
                )
        ]


viewStatementIdRatingPanel : I18n.Language -> (String -> msg) -> DataProxy a -> String -> Html msg
viewStatementIdRatingPanel language navigateMsg data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewStatementRatingPanel language navigateMsg (Just "cards") card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewStatementRatingPanel language navigateMsg (Just "properties") property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewStatementRatingPanel language navigateMsg (Just "affirmations") typedValue

                        Nothing ->
                            i [ class "text-warning" ] [ text ("Missing statement with ID: " ++ statementId) ]


viewStatementRatingPanel :
    I18n.Language
    -> (String -> msg)
    -> Maybe String
    -> { a | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingPanel language navigateMsg objectsUrlName { argumentCount, id, ratingCount, ratingSum, trashed } =
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
            case objectsUrlName of
                Just objectsUrlName ->
                    aForPath
                        navigateMsg
                        language
                        ("/" ++ objectsUrlName ++ "/" ++ id)
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


viewStatementRatingToolbar :
    I18n.Language
    -> (Maybe Int -> msg)
    -> msg
    -> DataProxy a
    -> { b | ballotId : String }
    -> Html msg
viewStatementRatingToolbar language rateMsg trashMsg data { ballotId } =
    div
        [ class "toolbar"
        , role "toolbar"
        ]
        [ let
            ballot =
                Dict.get ballotId data.ballots

            ballotRating =
                Maybe.map .rating ballot
          in
            div
                [ ariaLabel "Rating panel"
                , class "btn-group"
                , role "group"
                ]
                [ button
                    [ ariaPressed (ballotRating == Just 1)
                    , classList
                        [ ( "active", ballotRating == Just 1 )
                        , ( "btn", True )
                        , ( "btn-outline-success", True )
                        ]
                    , onClick
                        (if ballotRating == Just 1 then
                            rateMsg Nothing
                         else
                            rateMsg (Just 1)
                        )
                    , type_ "button"
                    ]
                    [ span
                        [ ariaHidden True
                        , class "fa fa-thumbs-o-up"
                        ]
                        []
                    , text " "
                    , text <|
                        I18n.translate
                            language
                            I18n.Agree
                    ]
                , button
                    [ ariaPressed (ballotRating == Just 0)
                    , classList
                        [ ( "active", ballotRating == Just 0 )
                        , ( "btn", True )
                        , ( "btn-outline-secondary", True )
                        ]
                    , onClick
                        (if ballotRating == Just 0 then
                            rateMsg Nothing
                         else
                            rateMsg (Just 0)
                        )
                    , type_ "button"
                    ]
                    [ span
                        [ ariaHidden True
                        , class "fa fa-square-o"
                        ]
                        []
                    , text " "
                    , text <|
                        I18n.translate
                            language
                            I18n.Abstain
                    ]
                , button
                    [ ariaPressed (ballotRating == Just -1)
                    , classList
                        [ ( "active", ballotRating == Just -1 )
                        , ( "btn", True )
                        , ( "btn-outline-danger", True )
                        ]
                    , onClick
                        (if ballotRating == Just -1 then
                            rateMsg Nothing
                         else
                            rateMsg (Just -1)
                        )
                    , type_ "button"
                    ]
                    [ span
                        [ ariaHidden True
                        , class "fa fa-thumbs-o-down"
                        ]
                        []
                    , text " "
                    , text <|
                        I18n.translate
                            language
                            I18n.Disagree
                    ]
                ]
        , text " "
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-danger", True )
                ]
            , onClick trashMsg
            , type_ "button"
            ]
            [ span
                [ ariaHidden True
                , class "fa fa-trash-o"
                ]
                []
            , text " "
            , text <|
                I18n.translate
                    language
                    I18n.Trash
            ]
        ]
