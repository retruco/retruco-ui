module Properties.Item.Types exposing (..)

import Arguments.New.Types
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = DebatePropertiesRetrieved (Result Http.Error DataIdsBody)
    | NewArgumentMsg Arguments.New.Types.InternalMsg
    | Rate String (Maybe Int)
    | RatingPosted (Result Http.Error DataIdBody)
    | Retrieve
    | Trash String
    | TrashUpserted (Result Http.Error DataIdBody)
    | Upserted Types.DataId
    | ValueRetrieved (Result Http.Error DataIdBody)
    | ValueUpdated (Result Http.Error DataIdBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , debatePropertyIds : Maybe (List String)
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language
    , newArgumentModel : Arguments.New.Types.Model
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


translateNewArgumentMsg : Arguments.New.Types.MsgTranslator Msg
translateNewArgumentMsg =
    Arguments.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewArgumentMsg
        , onPropertyUpserted = ForSelf << Upserted
        , onRequireSignIn = ForParent << RequireSignIn << NewArgumentMsg
        }
