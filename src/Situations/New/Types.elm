module Situations.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.New.Types
import Dict exposing (Dict)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = RequireSignIn InternalMsg
    | SituationUpserted DataId


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = NewCardMsg Cards.New.Types.InternalMsg
    | TypePropertyUpserted (Result Http.Error DataIdBody)
    | Upserted DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataId
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language
    , newCardModel : Cards.New.Types.Model
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    , onSituationUpserted : DataId -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onRequireSignIn, onSituationUpserted } msg =
    case msg of
        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForParent (SituationUpserted data) ->
            onSituationUpserted data

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewCardMsg : Cards.New.Types.MsgTranslator Msg
translateNewCardMsg =
    Cards.New.Types.translateMsg
        { onCardUpserted = ForSelf << Upserted
        , onInternalMsg = ForSelf << NewCardMsg
        , onRequireSignIn = ForParent << RequireSignIn << NewCardMsg
        }
