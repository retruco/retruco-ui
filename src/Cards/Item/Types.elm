module Cards.Item.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import DebateProperties.Index.Types
import Http
import I18n
import Properties.KeysAutocomplete.Types
import Properties.SameObjectAndKey.Types
import Statements.Toolbar.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = AddKey TypedValue
    | CardRetrieved (Result Http.Error DataIdBody)
    | CreateKey String
    | DataUpdated (DataProxy {})
    | DebatePropertiesMsg DebateProperties.Index.Types.InternalMsg
    | KeyUpserted (Result Http.Error DataIdBody)
    | KeysAutocompleteMsg Properties.KeysAutocomplete.Types.InternalMsg
    | Retrieve
    | SameObjectAndKeyPropertiesMsg Properties.SameObjectAndKey.Types.InternalMsg
    | ToolbarMsg Statements.Toolbar.Types.InternalMsg


type alias Model =
    { activeTab : Tab
    , authentication : Maybe Authentication
    , card : Maybe Card
    , data : DataProxy {}
    , id : String
    , keysAutocompleteModel : Properties.KeysAutocomplete.Types.Model
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , sameObjectAndKeyPropertiesModel : Maybe Properties.SameObjectAndKey.Types.Model
    , showTrashed : Bool
    , toolbarModel : Maybe (Statements.Toolbar.Types.Model Card)
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


type Tab
    = DebatePropertiesTab DebateProperties.Index.Types.Model
    | PropertiesTab


translateDebatePropertiesMsg : DebateProperties.Index.Types.MsgTranslator Msg
translateDebatePropertiesMsg =
    DebateProperties.Index.Types.translateMsg
        { onInternalMsg = ForSelf << DebatePropertiesMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << DebatePropertiesMsg
        }


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


translateSameObjectAndKeyPropertiesMsg : Properties.SameObjectAndKey.Types.MsgTranslator Msg
translateSameObjectAndKeyPropertiesMsg =
    Properties.SameObjectAndKey.Types.translateMsg
        { onInternalMsg = ForSelf << SameObjectAndKeyPropertiesMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << SameObjectAndKeyPropertiesMsg
        }


translateToolbarMsg : Statements.Toolbar.Types.MsgTranslator Msg
translateToolbarMsg =
    Statements.Toolbar.Types.translateMsg
        { onDataUpdated = ForSelf << DataUpdated
        , onInternalMsg = ForSelf << ToolbarMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << ToolbarMsg
        }
