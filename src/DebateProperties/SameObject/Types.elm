module DebateProperties.SameObject.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import DebateProperties.New.Types
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = NewArgumentMsg DebateProperties.New.Types.InternalMsg
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | Upserted Types.DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , debatePropertyIds : Maybe (List String)
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newDebatePropertyModel : DebateProperties.New.Types.Model
    , objectId : String
    , showTrashed : Bool
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate, onRequireSignIn } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewArgumentMsg : DebateProperties.New.Types.MsgTranslator Msg
translateNewArgumentMsg =
    DebateProperties.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewArgumentMsg
        , onPropertyUpserted = ForSelf << Upserted
        , onRequireSignIn = ForParent << RequireSignIn << NewArgumentMsg
        }