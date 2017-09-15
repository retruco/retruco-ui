module Cards.Index.View exposing (..)

import Array
import Cards.Index.Types exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import Http.Error
import I18n
import Json.Decode
import Statements.Lines exposing (viewCardIdLine)
import Views


searchSortLabelCouples : List ( String, I18n.TranslationId )
searchSortLabelCouples =
    [ ( "old", I18n.OldSortLabel )
    , ( "recent", I18n.RecentSortLabel )
    ]


searchSorts : List String
searchSorts =
    List.map (\( item, label ) -> item) searchSortLabelCouples


decodeSearchSort : String -> Json.Decode.Decoder String
decodeSearchSort value =
    if List.member value searchSorts then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search sort: " ++ value)


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        div []
            ([ nav
                [ class "bg-light navbar navbar-expand-sm navbar-light" ]
                [ div [ class "navbar-collapse" ]
                    [ Html.form [ class "form-inline mr-auto", onSubmit (ForSelf Submit) ]
                        [ viewInlineSearchSort
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
                                (ForParent << Navigate)
                                language
                                "/cards/new"
                                [ class "btn btn-secondary", role "button" ]
                                [ text <| I18n.translate language I18n.NewCard ]
                            ]
                        ]
                    ]
                ]
             ]
                ++ case model.ids of
                    Just ids ->
                        [ div [ class "list-group" ]
                            (Array.toList ids
                                |> List.map
                                    (\cardId ->
                                        aForPath
                                            (ForParent << Navigate)
                                            language
                                            ("/cards/" ++ cardId)
                                            [ class "list-group-item list-group-item-action" ]
                                            [ viewCardIdLine language data cardId ]
                                    )
                            )
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
                                            I18n.translate language I18n.ValuesRetrievalFailed
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


viewInlineSearchSort : I18n.Language -> String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchSort language searchSort errorMaybe searchSortChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "search-sort-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "search-sort-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "sr-only", for "search-sort" ] [ text "Type" ]
             , select
                ([ class "form-control"
                 , id "search-sort"
                 , on "change" (Json.Decode.map searchSortChanged (targetValue |> Json.Decode.andThen decodeSearchSort))
                 ]
                    ++ errorAttributes
                )
                (searchSortLabelCouples
                    |> List.map
                        (\( symbol, labelI18n ) ->
                            ( symbol
                            , I18n.translate language labelI18n
                            )
                        )
                    |> List.sortBy (\( symbol, label ) -> label)
                    |> List.map
                        (\( symbol, label ) ->
                            option
                                [ selected (symbol == searchSort)
                                , value symbol
                                ]
                                [ text label ]
                        )
                )
             ]
                ++ errorBlock
            )
