module Interventions.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Proposals.New.Types
import Types exposing (..)


type ExternalMsg
    = InterventionUpserted DataWithId
    | RequireSignIn InternalMsg


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = InterventionSpecificPropertyUpserted (Result Http.Error DataWithIdBody)
    | KeyIdChanged String
    | NewProposalMsg Proposals.New.Types.InternalMsg
    | ObjectUpserted DataWithId ObjectWrapper
    | Upserted (Result Http.Error DataWithIdBody)
    | ProposalUpserted DataWithId
    | Submit


type alias Model =
    { authentication : Maybe Authentication
    , data : DataWithId
    , embed : Bool
    , errors : FormErrors
    , httpError : Maybe Http.Error
    , keyId : String
    , keyIds : List String
    , language : I18n.Language
    , newProposalModel : Proposals.New.Types.Model
    , objectId : String -- Discussion ID
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onInterventionUpserted : DataWithId -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onInterventionUpserted, onRequireSignIn } msg =
    case msg of
        ForParent (InterventionUpserted data) ->
            onInterventionUpserted data

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewProposalMsg : Proposals.New.Types.MsgTranslator Msg
translateNewProposalMsg =
    Proposals.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewProposalMsg
        , onProposalUpserted = ForSelf << ProposalUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewProposalMsg
        }
