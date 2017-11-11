module Discussions.New.View exposing (..)

import Cards.New.View
import Discussions.New.Types exposing (..)
import Html exposing (..)
import I18n


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewDiscussion ]
        , viewForm I18n.Create I18n.CardCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Cards.New.View.viewForm submitButtonI18n creationFailedI18n model.newCardModel
        |> Html.map translateNewCardMsg
