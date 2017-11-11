module Discussions.NewSuggestion.View exposing (..)

import Discussions.NewSuggestion.Types exposing (..)
import Html exposing (..)
import I18n
import Proposals.New.View


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewSuggestion ]
        , viewForm I18n.Create I18n.SuggestionCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Proposals.New.View.viewForm I18n.Create I18n.SuggestionCreationFailed model.newProposalModel
        |> Html.map translateNewProposalMsg
