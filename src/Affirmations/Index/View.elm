module Affirmations.Index.View exposing (..)

import Affirmations.Index.Types exposing (..)
import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Statements.Lines exposing (lineIdAttributes, viewValueTypeLine)
import Statements.RatingPanels exposing (viewStatementRatingPanel)
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language

        navigateMsg =
            ForParent << Navigate
    in
        div []
            ([ nav
                [ class "bg-light navbar navbar-expand-sm navbar-light" ]
                [ div [ class "navbar-collapse" ]
                    [ Html.form [ class "form-inline mr-auto", onSubmit (ForSelf Submit) ]
                        [ Views.viewInlineSearchSort
                            language
                            model.searchSort
                            (Dict.get "searchSort" model.errors)
                            (ForSelf << SearchSortChanged)
                        , Views.viewInlineSearchTerm
                            language
                            model.searchTerm
                            (Dict.get "searchTerm" model.errors)
                            (ForSelf << SearchTermChanged)
                        , button [ class "btn btn-primary", type_ "submit" ]
                            [ span [ class "fa fa-search" ] []
                            , text " "
                            , text <| I18n.translate language I18n.Search
                            ]
                        ]
                    , text " "
                    , ul [ class "navbar-nav" ]
                        [ li [ class "nav-item" ]
                            [ aForPath
                                navigateMsg
                                language
                                "/affirmations/new"
                                [ class "btn btn-secondary", role "button" ]
                                [ text <| I18n.translate language I18n.NewAffirmation ]
                            ]
                        ]
                    ]
                ]
             ]
                ++ case model.ids of
                    Just ids ->
                        [ div []
                            [ ul [ class "list-group" ]
                                (Array.toList ids
                                    |> List.filterMap
                                        (\valueId ->
                                            case Dict.get valueId data.values of
                                                Just typedValue ->
                                                    Just <|
                                                        li
                                                            (lineIdAttributes
                                                                language
                                                                (Just navigateMsg)
                                                                [ ( "list-group-item", True ) ]
                                                                data
                                                                typedValue.id
                                                            )
                                                            [ div [ class "lead" ]
                                                                [ viewValueTypeLine
                                                                    language
                                                                    False
                                                                    data
                                                                    typedValue.value
                                                                ]
                                                            , viewStatementRatingPanel language data typedValue
                                                            ]

                                                Nothing ->
                                                    Nothing
                                        )
                                )
                            ]
                        , if Array.length ids < model.count then
                            button
                                [ class "btn btn-secondary btn-lg btn-block"
                                , onClick <| ForSelf <| Retrieve <| Array.length ids
                                , type_ "button"
                                ]
                                [ text <| I18n.translate language I18n.MoreButton ]
                          else
                            text ""
                        ]

                    Nothing ->
                        case model.httpError of
                            Just httpError ->
                                [ div
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
                                ]

                            Nothing ->
                                [ div [ class "text-center" ]
                                    [ Views.viewLoading language ]
                                ]
            )
