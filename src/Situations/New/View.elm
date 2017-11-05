module Situations.New.View exposing (..)

import Cards.New.View
import Html exposing (..)
import I18n
import Situations.New.Types exposing (..)


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewSituation ]
        , viewForm I18n.Create I18n.CardCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Cards.New.View.viewForm submitButtonI18n creationFailedI18n model.newCardModel
        |> Html.map translateNewCardMsg
