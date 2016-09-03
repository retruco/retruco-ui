module Views exposing (aForPath, viewGroundArgumentLinePanel, viewKind, viewLanguageCode, viewName, viewNotFound,
    viewOption, viewRating, viewStatementLinePanel)

import Json.Decode
import Html exposing (a, Attribute, div, Html, img, input, label, option, p, select, span, text)
import Html.Attributes exposing (class, for, placeholder, href, id, selected, src, type', value)
import Html.Attributes.Aria exposing (ariaDescribedby)
import Html.Events exposing (on, onInput, onWithOptions, targetValue)
import Routes exposing (makeUrl)
import String
import Types exposing (Statement)


kindLabelCouples : List (String, String)
kindLabelCouples =
    [ ("PlainStatement", "Plain")
    , ("Tag", "Tag")
    ]


kinds : List String
kinds = List.map (\(item, label) -> item) kindLabelCouples


languageCodeLabelCouples : List (String, String)
languageCodeLabelCouples =
    [ ("en", "English")
    , ("fr", "Français")
    ]


languageCodes : List String
languageCodes = List.map (\(item, label) -> item) languageCodeLabelCouples


ratingLabelCouples : List (Int, String)
ratingLabelCouples =
    [ (1, "Because")
    , (0, "However")
    , (-1, "But")
    ]


ratings : List Int
ratings = List.map (\(item, label) -> item) ratingLabelCouples


aForPath : (String -> msg) -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
aForPath navigate path attributes children =
    a
        (
            [ href (makeUrl path)
            , onWithOptions
                "click"
                { stopPropagation = False, preventDefault = True }
                (Json.Decode.succeed (navigate path))
            ]
            ++ attributes
        )
        children


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


decodeRating : Int -> Json.Decode.Decoder Int
decodeRating value =
    if List.member value ratings then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown rating: " ++ toString value)


decodeRatingTargetValue : Json.Decode.Decoder Int
decodeRatingTargetValue =
    Json.Decode.customDecoder targetValue (Json.Decode.decodeString Json.Decode.int) `Json.Decode.andThen` decodeRating


viewGroundArgumentLinePanel : Statement -> Html msg
viewGroundArgumentLinePanel statement =
    div [] [ text "viewGroundArgumentLinePanel TODO!" ]


viewKind : String -> Maybe String -> (String -> msg) -> Html msg
viewKind kind errorMaybe kindChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
            Just error ->
                ( " has-error"
                , [ ariaDescribedby "type-error" ]
                , [ span
                    [ class "help-block"
                    , id "type-error"
                    ]
                    [ text error ] ]
                )
            Nothing ->
                ("", [] , [])
    in
        div [ class ("form-group" ++ errorClass) ]
            ( [ label [ class "control-label", for "type" ] [ text "Type" ]
            , select
                ( [ class "form-control"
                , id "type"
                , on "change" (Json.Decode.map kindChanged (targetValue `Json.Decode.andThen` decodeKind))
                ] ++ errorAttributes )
                ( List.map
                    (viewOption kind)
                    kindLabelCouples
                )
            ] ++ errorBlock )


viewLanguageCode : String -> Maybe String -> (String -> msg) -> Html msg
viewLanguageCode languageCode errorMaybe languageCodeChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
            Just error ->
                ( " has-error"
                , [ ariaDescribedby "language-error" ]
                , [ span
                    [ class "help-block"
                    , id "language-error"
                    ]
                    [ text error ] ]
                )
            Nothing ->
                ("", [] , [])
    in
        div [ class ( "form-group" ++ errorClass) ]
            ( [ label [ class "control-label", for "language" ] [ text "Language" ]
            , select
                ( [ class "form-control"
                , id "language"
                , on "change" (Json.Decode.map languageCodeChanged
                    (targetValue `Json.Decode.andThen` decodeLanguageCode))
                ] ++ errorAttributes )
                ( List.map
                    (viewOption languageCode)
                    languageCodeLabelCouples
                )
            ] ++ errorBlock )


viewName : String -> Maybe String -> (String -> msg) -> Html msg
viewName name errorMaybe nameChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
            Just error ->
                ( " has-error"
                , [ ariaDescribedby "name-error" ]
                , [ span
                    [ class "help-block"
                    , id "name-error"
                    ]
                    [ text error ] ]
                )
            Nothing ->
                ("", [] , [])
    in
        div [ class ( "form-group" ++ errorClass) ]
            ( [ label [ class "control-label", for "name" ] [ text "Name" ]
            , input
                ( [ class "form-control"
                , id "name"
                , placeholder "To be or not to be"
                , type' "text"
                , value name
                , onInput nameChanged
                ] ++ errorAttributes )
                []
            ] ++ errorBlock )


viewNotFound : Html msg
viewNotFound =
    p
        []
        [ img [ src "./img/elm.png" ] []
        , text "Page not found!"
        ]


viewRating : Int -> Maybe String -> (Int -> msg) -> Html msg
viewRating rating errorMaybe ratingChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
            Just error ->
                ( " has-error"
                , [ ariaDescribedby "rating-error" ]
                , [ span
                    [ class "help-block"
                    , id "rating-error"
                    ]
                    [ text error ] ]
                )
            Nothing ->
                ("", [] , [])
    in
        div [ class ("form-group" ++ errorClass) ]
            ( [ label [ class "control-label", for "rating" ] [ text "Rating" ]
            , select
                ( [ class "form-control"
                , id "rating"
                , on "change" (Json.Decode.map ratingChanged decodeRatingTargetValue)
                ] ++ errorAttributes )
                ( List.map
                    (viewOption rating)
                    ratingLabelCouples
                )
            ] ++ errorBlock )

viewOption : a -> (a, String) -> Html msg
viewOption selectedItem (item, label) =
    let
        itemString = (toString item)
        itemString' = if String.left 1 itemString == "\"" && String.right 1 itemString == "\"" then
                String.slice 1 -1 itemString
            else
                itemString
    in
        option
            [ selected (item == selectedItem)
            , value itemString'
            ]
            [ text label ]


viewStatementLinePanel : Statement -> Html msg
viewStatementLinePanel statement =
    div [] [ text "viewStatementLinePanel TODO!" ]
