module Authenticator.Activate.View exposing (..)

import Authenticator.Activate.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Views


view : I18n.Language -> Model -> Html msg
view language model =
    let
        alert =
            case model.authentication of
                Just authentication ->
                    div
                        [ class "alert alert-success"
                        , role "alert"
                        ]
                        [ strong [] [ text <| I18n.translate language I18n.ActivationSucceeded ] ]

                Nothing ->
                    case model.httpError of
                        Just httpError ->
                            div
                                [ class "alert alert-danger"
                                , role "alert"
                                ]
                                [ strong []
                                    [ text <|
                                        I18n.translate language I18n.ActivationFailed
                                            ++ I18n.translate language I18n.Colon
                                    ]
                                , text <| Http.Error.toString language httpError
                                ]

                        Nothing ->
                            div [ class "text-center" ]
                                [ Views.viewLoading language ]
    in
        div []
            [ h1 [] [ text (I18n.translate language I18n.ActivationTitle) ]
            , alert
            ]


viewModalBody : I18n.Language -> Model -> Html Msg
viewModalBody language model =
    div [ class "modal-body" ]
        [ view language model ]
