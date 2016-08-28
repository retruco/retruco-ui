module Views exposing (..)

import Json.Decode
import Html exposing (a, Attribute, Html)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import Routes exposing (makeUrl)


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
