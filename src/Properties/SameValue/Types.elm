module Properties.SameValue.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Properties.KeysAutocomplete.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = Retrieve
    | Retrieved (Result Http.Error DataIdsBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , propertyIds : Maybe (List String)
    , showTrashed : Bool
    , valueId : String
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
