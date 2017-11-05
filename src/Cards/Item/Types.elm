module Cards.Item.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import DebateProperties.SameObject.Types
import Http
import I18n
import Properties.SameObject.Types
import Properties.SameObjectAndKey.Types
import Properties.SameValue.Types
import Statements.Toolbar.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = CardRetrieved (Result Http.Error DataIdBody)
    | DataUpdated Data
    | DebatePropertiesMsg DebateProperties.SameObject.Types.InternalMsg
    | DuplicatedByRetrieved (Result Http.Error DataIdsBody)
    | DuplicateOfRetrieved (Result Http.Error DataIdsBody)
    | PropertiesAsValueMsg Properties.SameValue.Types.InternalMsg
    | PropertiesMsg Properties.SameObject.Types.InternalMsg
    | Retrieve
    | SameKeyPropertiesMsg Properties.SameObjectAndKey.Types.InternalMsg
    | ToolbarMsg Statements.Toolbar.Types.InternalMsg


type alias Model =
    { activeTab : Tab
    , authentication : Maybe Authentication
    , card : Maybe Card
    , data : Data
    , duplicatedByPropertyIds : Maybe (List String)
    , duplicateOfPropertyIds : Maybe (List String)
    , id : String
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , sameKeyPropertiesModel : Maybe Properties.SameObjectAndKey.Types.Model
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
    = DebatePropertiesTab DebateProperties.SameObject.Types.Model
    | NoTab
    | PropertiesAsValueTab Properties.SameValue.Types.Model
    | PropertiesTab Properties.SameObject.Types.Model


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


translatePropertiesAsValueMsg : Properties.SameValue.Types.MsgTranslator Msg
translatePropertiesAsValueMsg =
    Properties.SameValue.Types.translateMsg
        { onInternalMsg = ForSelf << PropertiesAsValueMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << PropertiesAsValueMsg
        }


translatePropertiesMsg : Properties.SameObject.Types.MsgTranslator Msg
translatePropertiesMsg =
    Properties.SameObject.Types.translateMsg
        { onInternalMsg = ForSelf << PropertiesMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << PropertiesMsg
        }


translateSameKeyPropertiesMsg : Properties.SameObjectAndKey.Types.MsgTranslator Msg
translateSameKeyPropertiesMsg =
    Properties.SameObjectAndKey.Types.translateMsg
        { onInternalMsg = ForSelf << SameKeyPropertiesMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << SameKeyPropertiesMsg
        }


translateToolbarMsg : Statements.Toolbar.Types.MsgTranslator Msg
translateToolbarMsg =
    Statements.Toolbar.Types.translateMsg
        { onDataUpdated = ForSelf << DataUpdated
        , onInternalMsg = ForSelf << ToolbarMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << ToolbarMsg
        }
