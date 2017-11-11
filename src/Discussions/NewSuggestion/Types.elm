module Discussions.NewSuggestion.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Proposals.New.Types
import Types exposing (..)


type ExternalMsg
    = SuggestionUpserted DataId
    | RequireSignIn InternalMsg


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = NewProposalMsg Proposals.New.Types.InternalMsg
    | Upserted (Result Http.Error DataIdBody)
    | ProposalUpserted DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataId
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newProposalModel : Proposals.New.Types.Model
    , objectId : String -- Discussion ID
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onSuggestionUpserted : DataId -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onRequireSignIn, onSuggestionUpserted } msg =
    case msg of
        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForParent (SuggestionUpserted data) ->
            onSuggestionUpserted data

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewProposalMsg : Proposals.New.Types.MsgTranslator Msg
translateNewProposalMsg =
    Proposals.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewProposalMsg
        , onProposalUpserted = ForSelf << ProposalUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewProposalMsg
        }
