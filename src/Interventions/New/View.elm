module Interventions.New.View exposing (..)

import Html exposing (..)
import I18n
import Interventions.New.Types exposing (..)
import Proposals.New.View


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewIntervention ]
        , viewForm I18n.Create I18n.InterventionCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Proposals.New.View.viewForm I18n.Create I18n.InterventionCreationFailed model.newProposalModel
        |> Html.map translateNewProposalMsg
