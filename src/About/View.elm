module About.View exposing (..)

import About.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "jumbotron" ]
            [ h1 [ class "display-3" ]
                [ text "Retruco " ]
            , p
                []
                [ text "Bring out shared positions from argumented statements" ]
            ]
        , p [ class "mb-5 mt-5" ]
            [ text "This service is entirely made of free and open source software. For more informations and also to contribue, refer to "
            , a [ href "https://framagit.org/retruco/retruco-ui", target "_blank" ] [ text "FramaGit" ]
            , text "."
            ]
        ]
