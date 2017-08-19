module Properties.Item.Types exposing (..)

import Arguments.New.Types
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Statements.Toolbar.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = DataUpdated (DataProxy {})
    | DebatePropertiesRetrieved (Result Http.Error DataIdsBody)
    | DebatePropertyUpserted Types.DataId
    | NewArgumentMsg Arguments.New.Types.InternalMsg
    | Retrieve
    | ToolbarMsg Statements.Toolbar.Types.InternalMsg
    | ValueRetrieved (Result Http.Error DataIdBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , debatePropertyIds : Maybe (List String)
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language
    , newArgumentModel : Arguments.New.Types.Model
    , property : Maybe Property
    , showTrashed : Bool
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


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate, onRequireSignIn } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewArgumentMsg : Arguments.New.Types.MsgTranslator Msg
translateNewArgumentMsg =
    Arguments.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewArgumentMsg
        , onPropertyUpserted = ForSelf << DebatePropertyUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewArgumentMsg
        }


translateToolbarMsg : Statements.Toolbar.Types.MsgTranslator Msg
translateToolbarMsg =
    Statements.Toolbar.Types.translateMsg
        { onDataUpdated = ForSelf << DataUpdated
        , onInternalMsg = ForSelf << ToolbarMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << ToolbarMsg
        }
