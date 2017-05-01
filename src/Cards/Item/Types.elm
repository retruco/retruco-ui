module Cards.Item.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Properties.KeysAutocomplete.Types
import SameKeyProperties.Types
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
    | Retrieved (Result Http.Error DataIdBody)
    | SameKeyPropertiesMsg SameKeyProperties.Types.InternalMsg


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , id : String
    , keysAutocompleteModel : Properties.KeysAutocomplete.Types.Model
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , sameKeyPropertiesModel : Maybe SameKeyProperties.Types.Model
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


translateSameKeyPropertiesMsg : SameKeyProperties.Types.MsgTranslator Msg
translateSameKeyPropertiesMsg =
    SameKeyProperties.Types.translateMsg
        { onInternalMsg = ForSelf << SameKeyPropertiesMsg
        , onNavigate = ForParent << Navigate
        }
