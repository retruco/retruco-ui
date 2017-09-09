module Properties.SameObject.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Properties.KeysAutocomplete.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = AddKey TypedValue
    | CreateKey String
    | KeyUpserted (Result Http.Error DataIdBody)
    | KeysAutocompleteMsg Properties.KeysAutocomplete.Types.InternalMsg
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | Upserted Types.DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , keysAutocompleteModel : Properties.KeysAutocomplete.Types.Model
    , language : I18n.Language
    , objectId : String
    , propertyIds : Maybe (Array String)
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


translateKeysAutocompleteMsg : Properties.KeysAutocomplete.Types.MsgTranslator Msg
translateKeysAutocompleteMsg =
    Properties.KeysAutocomplete.Types.translateMsg
        { onAdd = ForSelf << AddKey
        , onCreate = ForSelf << CreateKey
        , onInternalMsg = ForSelf << KeysAutocompleteMsg
        }


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate, onRequireSignIn } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg
