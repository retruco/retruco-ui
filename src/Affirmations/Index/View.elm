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
import LineViews exposing (viewValueTypeLine)
import Statements.ViewsHelpers exposing (viewStatementRatingPanel)
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
                                            Just <|
                                                li
                                                    [ class "d-flex flex-nowrap justify-content-between list-group-item"
                                                    ]
                                                    [ viewValueTypeLine
                                                        language
                                                        (Just (ForParent << Navigate))
                                                        False
                                                        data
                                                        typedValue.value
                                                    , viewStatementRatingPanel
                                                        language
                                                        (ForParent << Navigate)
                                                        (Just "affirmations")
                                                        typedValue
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
