module DebateProperties.SameObject.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import DebateProperties.New.Types
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = NewDebatePropertyMsg DebateProperties.New.Types.InternalMsg
    | Retrieve
    | Retrieved (Result Http.Error DataWithIdsBody)
    | Upserted Types.DataWithId


type alias Model =
    { authentication : Maybe Authentication
    , data : Data
    , debatePropertyIds : Maybe (Array String)
    , embed : Bool
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


translateNewDebatePropertyMsg : DebateProperties.New.Types.MsgTranslator Msg
translateNewDebatePropertyMsg =
    DebateProperties.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewDebatePropertyMsg
        , onPropertyUpserted = ForSelf << Upserted
        , onRequireSignIn = ForParent << RequireSignIn << NewDebatePropertyMsg
        }
