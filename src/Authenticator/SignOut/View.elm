module Authenticator.SignOut.View exposing (..)

import Authenticator.SignOut.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import I18n


view : I18n.Language -> Model -> Html Msg
view language model =
    Html.form [ onSubmit (ForSelf <| Submit) ]
        [ div [ class "form-group" ]
            [ button
                [ class "btn btn-primary"
                , type_ "submit"
                ]
                [ text (I18n.translate language I18n.SignOut) ]
            , text " "
            , button
                [ class "btn btn-warning float-right"
                , type_ "button"
                , onClick (ForSelf <| Cancel)
                ]
                [ text (I18n.translate language I18n.Cancel) ]
            ]
        ]


viewModalBody : I18n.Language -> Model -> Html Msg
viewModalBody language model =
    div [ class "modal-body" ]
        [ view language model ]
