module Views exposing (aForPath, viewNotFound, viewStatementLinePanel)

import Json.Decode
import Html exposing (a, Attribute, div, Html, img, p, text)
import Html.Attributes exposing (href, src)
import Html.Events exposing (onWithOptions)
import Routes exposing (makeUrl)
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


viewNotFound : Html msg
viewNotFound =
    p
        []
        [ img [ src "./img/elm.png" ] []
        , text "Page not found!"
        ]


viewStatementLinePanel : Statement -> Html msg
viewStatementLinePanel statement =
    -- text "Panel TODO"
    div [] [ text "Panel TODO!" ]
