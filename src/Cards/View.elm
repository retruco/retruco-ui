module Cards.View exposing (..)

import Cards.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Json.Decode
import Objects.ViewsParts exposing (..)
import Views
import WebData


searchSortLabelCouples : List ( String, String )
searchSortLabelCouples =
    [ ( "first", "First" )
    , ( "latest", "Latest" )
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
        language =
            model.language
    in
        div []
            [ nav
                [ class "navbar navbar-light bg-faded" ]
                [ ul [ class "nav navbar-nav float-lg-right" ]
                    [ li [ class "nav-item" ]
                        [ aForPath
                            (ForParent << Navigate)
                            language
                            "/cards/new"
                            [ class "btn btn-secondary", role "button" ]
                            [ text <| I18n.translate language I18n.NewCard ]
                        ]
                    ]
                , Html.form [ class "form-inline", onSubmit (ForSelf Submit) ]
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
                    , button [ class "btn btn-primary", type_ "button" ]
                        [ span [ class "fa fa-search" ] []
                        , text " "
                        , text <| I18n.translate language I18n.Search
                        ]
                    ]
                ]
            , Views.viewWebData
                language
                (\loadingStatus ->
                    case loadingStatus of
                        WebData.Loading _ ->
                            div [ class "text-center" ]
                                [ Views.viewLoading language ]

                        WebData.Loaded body ->
                            div [ class "list-group" ]
                                (List.map
                                    (\cardId ->
                                        aForPath
                                            (ForParent << Navigate)
                                            language
                                            ("/cards/" ++ cardId)
                                            [ class "list-group-item list-group-item-action" ]
                                            [ viewCardIdLine language Nothing body.data cardId ]
                                    )
                                    body.data.ids
                                )
                )
                model.webData
            ]


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
                (List.map
                    (Views.viewOption searchSort)
                    searchSortLabelCouples
                )
             ]
                ++ errorBlock
            )
