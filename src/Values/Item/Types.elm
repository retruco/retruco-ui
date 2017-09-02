module Values.Item.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import DebateProperties.SameObject.Types
import Http
import I18n
import Properties.SameObjectAndKey.Types
import Statements.Toolbar.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = DataUpdated (DataProxy {})
    | DebatePropertiesMsg DebateProperties.SameObject.Types.InternalMsg
    | Retrieve
    | SameObjectAndKeyPropertiesMsg Properties.SameObjectAndKey.Types.InternalMsg
    | ToolbarMsg Statements.Toolbar.Types.InternalMsg
    | ValueRetrieved (Result Http.Error DataIdBody)


type alias Model =
    { activeTab : Tab
    , authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language
    , sameObjectAndKeyPropertiesModel : Maybe Properties.SameObjectAndKey.Types.Model
    , showTrashed : Bool
    , toolbarModel : Maybe (Statements.Toolbar.Types.Model TypedValue)
    , typedValue : Maybe TypedValue
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
    = DebatePropertiesTab DebateProperties.SameObject.Types.Model
    | DetailsTab
    | PropertiesTab


translateDebatePropertiesMsg : DebateProperties.SameObject.Types.MsgTranslator Msg
translateDebatePropertiesMsg =
    DebateProperties.SameObject.Types.translateMsg
        { onInternalMsg = ForSelf << DebatePropertiesMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << DebatePropertiesMsg
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
