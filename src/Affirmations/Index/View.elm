module Affirmations.Index.View exposing (..)

import Affirmations.Index.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Values.ViewsHelpers exposing (viewValueTypeLine)
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        div []
            [ nav
                [ class "bg-light navbar navbar-expand-sm navbar-light" ]
                [ div [ class "navbar-collapse" ]
                    [ Html.form [ class "form-inline mr-auto", onSubmit (ForSelf Submit) ]
                        [ Views.viewInlineSearchSort
                            -- language
                            model.searchSort
                            (Dict.get "searchSort" model.errors)
                            (ForSelf << SearchSortChanged)
                        , Views.viewInlineSearchTerm
                            language
                            model.searchTerm
                            (Dict.get "searchTerm" model.errors)
                            (ForSelf << SearchTermChanged)
                        , button [ class "btn btn-primary", type_ "button" ]
                            [ span [ class "fa fa-search" ] []
                            , text " "
                            , text <| I18n.translate language I18n.Search
                            ]
                        ]
                    , text " "
                    , ul [ class "navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ aForPath
                                (ForParent << Navigate)
                                language
                                "/affirmations/new"
                                [ class "btn btn-secondary", role "button" ]
                                [ text <| I18n.translate language I18n.NewAffirmation ]
                            ]
                        ]
                    ]
                ]
            , case model.ids of
                Just ids ->
                    div []
                        [ ul [ class "list-group" ]
                            (List.filterMap
                                (\valueId ->
                                    case Dict.get valueId data.values of
                                        Just typedValue ->
                                            let
                                                ballot =
                                                    Dict.get typedValue.ballotId data.ballots

                                                ballotRating =
                                                    Maybe.map .rating ballot
                                            in
                                                Just <|
                                                    li [ class "flex-nowrap justify-content-between list-group-item" ]
                                                        [ viewValueTypeLine
                                                            language
                                                            (Just (ForParent << Navigate))
                                                            data
                                                            False
                                                            typedValue.value
                                                        , div []
                                                            [ aForPath
                                                                (ForParent << Navigate)
                                                                language
                                                                ("/affirmations/" ++ typedValue.id)
                                                                [ class "btn btn-secondary" ]
                                                                [ text (I18n.translate language (I18n.Debate)) ]
                                                            , div
                                                                [ class "btn-group-vertical"
                                                                , role "group"
                                                                , ariaLabel "Rating panel"
                                                                ]
                                                                [ button
                                                                    [ ariaPressed (ballotRating == Just 1)
                                                                    , classList
                                                                        [ ( "active", ballotRating == Just 1 )
                                                                        , ( "btn", True )
                                                                        , ( "btn-secondary", True )
                                                                        ]
                                                                    , onClick
                                                                        (ForSelf
                                                                            (if ballotRating == Just 1 then
                                                                                UnvoteRating typedValue.id
                                                                             else
                                                                                VoteRatingUp typedValue.id
                                                                            )
                                                                        )
                                                                    , type_ "button"
                                                                    ]
                                                                    [ span
                                                                        [ ariaHidden True
                                                                        , class "fa fa-thumbs-o-up"
                                                                        ]
                                                                        []
                                                                    , span
                                                                        [ class "sr-only" ]
                                                                        [ text <|
                                                                            I18n.translate
                                                                                language
                                                                                I18n.GivePositiveRating
                                                                        ]
                                                                    ]
                                                                , button
                                                                    [ class "btn btn-secondary"
                                                                    , disabled True
                                                                    , type_ "button"
                                                                    ]
                                                                    [ text
                                                                        ((toString typedValue.ratingSum)
                                                                            ++ " / "
                                                                            ++ (toString typedValue.ratingCount)
                                                                        )
                                                                    ]
                                                                , button
                                                                    [ ariaPressed (ballotRating == Just -1)
                                                                    , classList
                                                                        [ ( "active", ballotRating == Just -1 )
                                                                        , ( "btn", True )
                                                                        , ( "btn-secondary", True )
                                                                        ]
                                                                    , onClick
                                                                        (ForSelf
                                                                            (if ballotRating == Just -1 then
                                                                                UnvoteRating typedValue.id
                                                                             else
                                                                                VoteRatingDown typedValue.id
                                                                            )
                                                                        )
                                                                    , type_ "button"
                                                                    ]
                                                                    [ span
                                                                        [ ariaHidden True
                                                                        , class "fa fa-thumbs-o-down"
                                                                        ]
                                                                        []
                                                                    , span
                                                                        [ class "sr-only" ]
                                                                        [ text <|
                                                                            I18n.translate
                                                                                language
                                                                                I18n.GiveNegativeRating
                                                                        ]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]

                                        Nothing ->
                                            Nothing
                                )
                                ids
                            )
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
                                        I18n.translate language I18n.AffirmationsRetrievalFailed
                                            ++ I18n.translate language I18n.Colon
                                    ]
                                , text <| Http.Error.toString language httpError
                                ]

                        Nothing ->
                            div [ class "text-center" ]
                                [ Views.viewLoading language ]
            ]
