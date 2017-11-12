module Questions.New.View exposing (..)

import Html exposing (..)
import I18n
import Questions.New.Types exposing (..)
import Proposals.New.View


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewQuestion ]
        , viewForm I18n.Create I18n.QuestionCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    Proposals.New.View.viewForm I18n.Create I18n.QuestionCreationFailed model.newProposalModel
        |> Html.map translateNewProposalMsg
