module Properties.SameObjectAndKey.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)
import Values.New.Types


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = NewValueMsg Values.New.Types.InternalMsg
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | Upserted (Result Http.Error DataIdBody)
    | ValueUpserted Types.DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , keyId : String
    , language : I18n.Language
    , newValueModel : Values.New.Types.Model
    , objectId : String
    , propertyIds : Maybe (Array String)
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


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewValueMsg
        , onRequireSignIn = ForParent << RequireSignIn << NewValueMsg
        , onValueUpserted = ForSelf << ValueUpserted
        }
