module Properties.Item.Types exposing (..)

import Arguments.Index.Types
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import SameKeyProperties.Types
import Statements.Toolbar.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = DataUpdated (DataProxy {})
    | ArgumentsMsg Arguments.Index.Types.InternalMsg
    | Retrieve
    | SameKeyPropertiesMsg SameKeyProperties.Types.InternalMsg
    | SimilarDebatePropertiesRetrieved (Result Http.Error DataIdsBody)
    | ToolbarMsg Statements.Toolbar.Types.InternalMsg
    | ValueRetrieved (Result Http.Error DataIdBody)


type alias Model =
    { activeTab : Tab
    , authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language
    , property : Maybe Property
    , sameKeyPropertiesModel : Maybe SameKeyProperties.Types.Model
    , showTrashed : Bool
    , similarDebatePropertyIds : Maybe (List String)
    , toolbarModel : Maybe (Statements.Toolbar.Types.Model Property)
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
    = DebatePropertiesTab Arguments.Index.Types.Model
    | PropertiesTab


translateArgumentsMsg : Arguments.Index.Types.MsgTranslator Msg
translateArgumentsMsg =
    Arguments.Index.Types.translateMsg
        { onInternalMsg = ForSelf << ArgumentsMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << ArgumentsMsg
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
