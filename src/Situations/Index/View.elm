module Situations.Index.View exposing (..)

import Array
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Situations.Index.Types exposing (..)
import Statements.Lines exposing (viewStatementIdRatedListGroupLine)
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
                                "/situations/new"
                                [ class "btn btn-secondary", role "button" ]
                                [ text <| I18n.translate language I18n.NewSituation ]
                            ]
                        ]
                    ]
                ]
             ]
                ++ case model.ids of
                    Just ids ->
                        [ div []
                            [ div [ class "list-group" ]
                                (Array.toList ids
                                    |> List.map
                                        (\cardId ->
                                            viewStatementIdRatedListGroupLine
                                                language
                                                navigateMsg
                                                "/situation"
                                                []
                                                True
                                                data
                                                cardId
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
                                            I18n.translate language I18n.SituationsRetrievalFailed
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
