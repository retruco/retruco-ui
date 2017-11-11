module Discussions.Item.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Discussions.NewIntervention.Types
import Http
import I18n
import Json.Encode
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = InterventionUpserted Types.DataId
    | NewInterventionMsg Discussions.NewIntervention.Types.InternalMsg
    | PropertyUpserted Json.Encode.Value
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : Data
    , discussionPropertyIds : Maybe (Array String)
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newInterventionModel : Discussions.NewIntervention.Types.Model
    , objectId : String
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


translateNewInterventionMsg : Discussions.NewIntervention.Types.MsgTranslator Msg
translateNewInterventionMsg =
    Discussions.NewIntervention.Types.translateMsg
        { onInternalMsg = ForSelf << NewInterventionMsg
        , onInterventionUpserted = ForSelf << InterventionUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewInterventionMsg
        }
