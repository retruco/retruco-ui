module Views exposing (aForPath, viewGroundArgumentLinePanel, viewNotFound, viewOption, viewStatementLinePanel)

import Json.Decode
import Html exposing (a, Attribute, div, Html, img, option, p, text)
import Html.Attributes exposing (href, selected, src, value)
import Html.Events exposing (onWithOptions)
import Routes exposing (makeUrl)
import String
import Types exposing (Statement)


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


viewGroundArgumentLinePanel : Statement -> Html msg
viewGroundArgumentLinePanel statement =
    div [] [ text "viewGroundArgumentLinePanel TODO!" ]


viewNotFound : Html msg
viewNotFound =
    p
        []
        [ img [ src "./img/elm.png" ] []
        , text "Page not found!"
        ]


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
