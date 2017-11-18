module Proposals.New.View exposing (..)

import Html exposing (..)
import I18n
import Proposals.New.Types exposing (..)
import Values.New.View


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewProposal ]
        , viewForm I18n.Create I18n.ProposalCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Values.New.View.viewForm submitButtonI18n creationFailedI18n model.newValueModel
        |> Html.map translateNewValueMsg


viewFormControls : Model -> List (Html Msg)
viewFormControls model =
    Values.New.View.viewFormControls model.newValueModel
        |> List.map (Html.map translateNewValueMsg)
