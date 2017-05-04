module Views exposing (..)

import Json.Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions, targetValue)
import Http exposing (Error(..))
import Http.Error
import I18n
import String
import Types exposing (..)
import WebData exposing (LoadingStatus, WebData(..))


argumentTypeLabelCouples : List ( String, String )
argumentTypeLabelCouples =
    [ ( "because", "Because" )
    , ( "but", "But" )
    , ( "comment", "Comment" )
    , ( "example", "Example" )
    ]


argumentTypes : List String
argumentTypes =
    List.map (\( item, label ) -> item) argumentTypeLabelCouples


kindLabelCouples : List ( String, String )
kindLabelCouples =
    [ ( "Citation", "Citation" )
    , ( "Event", "Event" )
    , ( "Person", "Person" )
    , ( "PlainStatement", "Plain" )
    , ( "Tag", "Tag" )
    ]


kinds : List String
kinds =
    List.map (\( item, label ) -> item) kindLabelCouples


languageCodeLabelCouples : List ( String, String )
languageCodeLabelCouples =
    [ ( "en", "English" )
    , ( "es", "Spanish" )
    , ( "fr", "French" )
    ]


languageCodes : List String
languageCodes =
    List.map (\( item, label ) -> item) languageCodeLabelCouples


searchLanguageCodeLabelCouples : List ( String, String )
searchLanguageCodeLabelCouples =
    [ ( "", "Any language" ) ] ++ languageCodeLabelCouples


searchLanguageCodes : List String
searchLanguageCodes =
    List.map (\( item, label ) -> item) searchLanguageCodeLabelCouples


searchSortLabelCouples : List ( String, String )
searchSortLabelCouples =
    [ ( "Popular", "Popular" )
    , ( "Recent", "Recent" )
    , ( "Trending", "Trending" )
    ]


searchSorts : List String
searchSorts =
    List.map (\( item, label ) -> item) searchSortLabelCouples


searchTypeLabelCouples : List ( String, String )
searchTypeLabelCouples =
    [ ( "", "Everything" )
    , ( "Citation", "Citations" )
    , ( "Event", "Events" )
    , ( "Person", "Persons" )
    , ( "PlainStatement", "Statements" )
    ]


searchTypes : List String
searchTypes =
    List.map (\( item, label ) -> item) searchTypeLabelCouples


decodeArgumentType : String -> Json.Decode.Decoder String
decodeArgumentType value =
    if List.member value argumentTypes then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown argument type: " ++ value)


decodeKind : String -> Json.Decode.Decoder String
decodeKind value =
    if List.member value kinds then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown type: " ++ value)


decodeLanguageCode : String -> Json.Decode.Decoder String
decodeLanguageCode value =
    if List.member value languageCodes then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown language: " ++ value)



-- decodeRating : Int -> Json.Decode.Decoder Int
-- decodeRating value =
--     if List.member value ratings then
--         Json.Decode.succeed value
--     else
--         Json.Decode.fail ("Unknown rating: " ++ toString value)
-- decodeRatingTargetValue : Json.Decode.Decoder Int
-- decodeRatingTargetValue =
--     Json.Decode.customDecoder targetValue (Json.Decode.decodeString Json.Decode.int) `Json.Decode.andThen` decodeRating


decodeSearchLanguageCode : String -> Json.Decode.Decoder String
decodeSearchLanguageCode value =
    if List.member value searchLanguageCodes then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search language: " ++ value)


decodeSearchSort : String -> Json.Decode.Decoder String
decodeSearchSort value =
    if List.member value searchSorts then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search sort: " ++ value)


decodeSearchType : String -> Json.Decode.Decoder String
decodeSearchType value =
    if List.member value searchTypes then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search type: " ++ value)


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


hasBallotRating : Int -> Maybe Ballot -> Bool
hasBallotRating rating ballotMaybe =
    case ballotMaybe of
        Just ballot ->
            ballot.rating == rating

        Nothing ->
            False


viewArgumentType : String -> Maybe String -> (String -> msg) -> Html msg
viewArgumentType argumentType errorMaybe argumentTypeChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "argument-type-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "argument-type-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "control-label", for "argument-type" ] [ text "Argument Type" ]
             , select
                ([ class "form-control"
                 , id "argument-type"
                 , on "change"
                    (Json.Decode.map argumentTypeChanged
                        (targetValue |> Json.Decode.andThen decodeArgumentType)
                    )
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (viewOption argumentType)
                    ([ ( "", "" ) ] ++ argumentTypeLabelCouples)
                )
             ]
                ++ errorBlock
            )


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



-- viewEvent : EventForm -> FormErrors -> (EventForm -> msg) -> Html msg
-- viewEvent eventForm errors eventChanged =
--     div [] [ text "TODO: Event" ]


viewI18nOption : I18n.Language -> a -> ( a, I18n.TranslationId ) -> Html msg
viewI18nOption language selectedItem ( item, label ) =
    let
        itemString =
            (toString item)

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
            [ text <| I18n.translate language label ]


viewInlineSearchLanguageCode : String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchLanguageCode searchLanguageCode errorMaybe searchLanguageCodeChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "search-language-code-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "search-language-code-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "sr-only", for "search-language-code" ] [ text "Type" ]
             , select
                ([ class "form-control"
                 , id "search-language-code"
                 , on "change"
                    (Json.Decode.map searchLanguageCodeChanged
                        (targetValue |> Json.Decode.andThen decodeSearchLanguageCode)
                    )
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (viewOption searchLanguageCode)
                    searchLanguageCodeLabelCouples
                )
             ]
                ++ errorBlock
            )


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


viewInlineSearchType : String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchType searchType errorMaybe searchTypeChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "search-type-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "search-type-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "sr-only", for "search-type" ] [ text "Type" ]
             , select
                ([ class "form-control"
                 , id "search-type"
                 , on "change" (Json.Decode.map searchTypeChanged (targetValue |> Json.Decode.andThen decodeSearchType))
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (viewOption searchType)
                    searchTypeLabelCouples
                )
             ]
                ++ errorBlock
            )


viewKind : String -> Maybe String -> (String -> msg) -> Html msg
viewKind kind errorMaybe kindChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "type-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "type-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "control-label", for "type" ] [ text "Type" ]
             , select
                ([ class "form-control"
                 , id "type"
                 , on "change" (Json.Decode.map kindChanged (targetValue |> Json.Decode.andThen decodeKind))
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (viewOption kind)
                    kindLabelCouples
                )
             ]
                ++ errorBlock
            )


viewLanguageCode : String -> Maybe String -> (String -> msg) -> Html msg
viewLanguageCode languageCode errorMaybe languageCodeChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "language-code-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "language-code-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "control-label", for "language-code" ] [ text "Language" ]
             , select
                ([ class "form-control"
                 , id "language-code"
                 , on "change"
                    (Json.Decode.map languageCodeChanged
                        (targetValue |> Json.Decode.andThen decodeLanguageCode)
                    )
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (viewOption languageCode)
                    languageCodeLabelCouples
                )
             ]
                ++ errorBlock
            )


viewLoading : I18n.Language -> Html msg
viewLoading language =
    div [ style [ ( "height", "100em" ) ] ]
        [ img [ class "loader", src "/img/loader.gif" ] [] ]


viewName : String -> String -> String -> Maybe String -> (String -> msg) -> Html msg
viewName fieldLabel fieldId fieldValue errorMaybe valueChanged =
    let
        errorId =
            fieldId ++ "-error"

        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby errorId ]
                    , [ div
                            [ class "form-control-feedback"
                            , id errorId
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "control-label", for fieldId ] [ text fieldLabel ]
             , input
                ([ class "form-control"
                 , id fieldId
                 , placeholder "To be or not to be"
                 , type_ "text"
                 , value fieldValue
                 , onInput valueChanged
                 ]
                    ++ errorAttributes
                )
                []
             ]
                ++ errorBlock
            )


viewNotAuthentified : I18n.Language -> Html msg
viewNotAuthentified language =
    viewBigMessage
        (I18n.translate language I18n.AuthenticationRequired)
        (I18n.translate language I18n.AuthenticationRequiredExplanation)


viewNotFound : I18n.Language -> Html msg
viewNotFound language =
    viewBigMessage
        (I18n.translate language I18n.PageNotFound)
        (I18n.translate language I18n.PageNotFoundExplanation)


viewOption : a -> ( a, String ) -> Html msg
viewOption selectedItem ( item, label ) =
    let
        itemString =
            (toString item)

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


viewTwitterName : String -> String -> String -> Maybe String -> (String -> msg) -> Html msg
viewTwitterName fieldLabel fieldId fieldValue errorMaybe valueChanged =
    let
        errorId =
            fieldId ++ "-error"

        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby errorId ]
                    , [ div
                            [ class "form-control-feedback"
                            , id errorId
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "control-label", for fieldId ] [ text fieldLabel ]
             , input
                ([ class "form-control"
                 , id fieldId
                 , placeholder "@JohnDoe"
                 , type_ "text"
                 , value fieldValue
                 , onInput valueChanged
                 ]
                    ++ errorAttributes
                )
                []
             ]
                ++ errorBlock
            )


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
                        BadPayload message response ->
                            genericTitle

                        BadStatus response ->
                            if response.status.code == 404 then
                                I18n.translate language I18n.PageNotFound
                            else
                                -- TODO Add I18n.BadStatusExplanation prefix
                                genericTitle

                        BadUrl message ->
                            genericTitle

                        NetworkError ->
                            genericTitle

                        Timeout ->
                            genericTitle
            in
                viewBigMessage title (Http.Error.toString language err)

        Data loadingStatus ->
            viewSuccess loadingStatus
