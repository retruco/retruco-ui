module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import I18n
import Json.Decode


searchSortLabelCouples : List ( String, I18n.TranslationId )
searchSortLabelCouples =
    [ ( "old", I18n.OldSortLabel )
    , ( "popular", I18n.PopularSortLabel )
    , ( "recent", I18n.RecentSortLabel )
    , ( "trending", I18n.TrendingSortLabel )
    ]


searchSorts : List String
searchSorts =
    List.map (\( item, _ ) -> item) searchSortLabelCouples


decodeSearchSort : String -> Json.Decode.Decoder String
decodeSearchSort value =
    if List.member value searchSorts then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search sort: " ++ value)


errorInfos : I18n.Language -> String -> Maybe I18n.TranslationId -> ( String, List (Attribute msg), List (Html msg1) )
errorInfos language fieldId error =
    let
        errorId =
            fieldId ++ "-error"
    in
        case error of
            Just error ->
                ( " has-danger"
                , [ ariaDescribedby errorId ]
                , [ div
                        [ class "form-control-feedback"
                        , id errorId
                        ]
                        [ text <| I18n.translate language error ]
                  ]
                )

            Nothing ->
                ( "", [], [] )


viewBigMessage : String -> String -> Html msg
viewBigMessage title message =
    div
        -- [ style
        --     [ ( "justify-content", "center" )
        --     , ( "flex-direction", "column" )
        --     , ( "display", "flex" )
        --     , ( "align-items", "center" )
        --     , ( "height", "100%" )
        --     , ( "margin", "1em" )
        --     , ( "font-family", "sans-serif" )
        --     ]
        -- ]
        []
        [ h1 []
            [ text title ]
        , p
            -- [ style
            --     [ ( "color", "rgb(136, 136, 136)" )
            --     , ( "margin-top", "3em" )
            --     ]
            -- ]
            []
            [ text message ]
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


viewInlineSearchTerm : I18n.Language -> String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchTerm language searchTerm errorMaybe searchTermChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "search-term-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "search-term-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "sr-only", for "search-term" ] [ text <| I18n.translate language I18n.SearchPlaceholder ]
             , input
                ([ class "form-control"
                 , id "search-term"
                 , placeholder <| I18n.translate language I18n.SearchPlaceholder
                 , type_ "text"
                 , value searchTerm
                 , onInput searchTermChanged
                 ]
                    ++ errorAttributes
                )
                []
             ]
                ++ errorBlock
            )


viewLoading : I18n.Language -> Html msg
viewLoading _ =
    div [ style [ ( "height", "100em" ) ] ]
        [ img [ class "loader", src "/img/loader.gif" ] [] ]


viewNotFound : I18n.Language -> Html msg
viewNotFound language =
    viewBigMessage
        (I18n.translate language I18n.PageNotFound)
        (I18n.translate language I18n.PageNotFoundExplanation)
