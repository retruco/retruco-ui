module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (on, onInput, targetValue)
import Http exposing (Error(..))
import Http.Error
import I18n
import Json.Decode
import String
import WebData exposing (LoadingStatus, WebData(..))


searchSortLabelCouples : List ( String, String )
searchSortLabelCouples =
    [ ( "popular", "Popular" )
    , ( "old", "Old" )
    , ( "recent", "Recent" )
    , ( "trending", "Trending" )
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


viewInlineSearchSort : String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchSort searchSort errorMaybe searchSortChanged =
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
                    (viewOption searchSort)
                    searchSortLabelCouples
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


viewOption : a -> ( a, String ) -> Html msg
viewOption selectedItem ( item, label ) =
    let
        itemString =
            toString item

        itemString_ =
            if String.left 1 itemString == "\"" && String.right 1 itemString == "\"" then
                String.slice 1 -1 itemString
            else
                itemString
    in
        option
            [ selected (item == selectedItem)
            , value itemString_
            ]
            [ text label ]


viewWebData : I18n.Language -> (LoadingStatus a -> Html msg) -> WebData a -> Html msg
viewWebData language viewSuccess webData =
    case webData of
        NotAsked ->
            div [ class "text-center" ]
                [ viewLoading language ]

        Failure err ->
            let
                genericTitle =
                    I18n.translate language I18n.GenericError

                title =
                    case err of
                        BadPayload _ _ ->
                            genericTitle

                        BadStatus response ->
                            if response.status.code == 404 then
                                I18n.translate language I18n.PageNotFound
                            else
                                -- TODO Add I18n.BadStatusExplanation prefix
                                genericTitle

                        BadUrl _ ->
                            genericTitle

                        NetworkError ->
                            genericTitle

                        Timeout ->
                            genericTitle
            in
                viewBigMessage title (Http.Error.toString language err)

        Data loadingStatus ->
            viewSuccess loadingStatus
