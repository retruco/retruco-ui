module About.View exposing (..)

import About.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n


view : Model -> Html Msg
view model =
    let
        language =
            model.language
    in
        div [ class "text-center" ]
            [ img
                [ alt <| I18n.translate language I18n.RetrucoLogo
                , class "img-fluid w-50"
                , src "/img/logo-1503x975.png"
                ]
                []
            , p
                [ class "lead mb-5 mt-5" ]
                [ text "Bring out shared positions from argumented statements" ]
            , p [ class "mb-5 mt-5" ]
                [ text "This service is entirely made of free and open source software. For more informations and also to contribue, refer to "
                , a [ href "https://framagit.org/retruco/retruco-ui", target "_blank" ] [ text "FramaGit" ]
                , text "."
                ]
            ]
