module Ideas.Index.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Ideas.New.Types
import Json.Encode
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = IdeaUpserted Types.DataId
    | NewIdeaMsg Ideas.New.Types.InternalMsg
    | PropertyUpserted Json.Encode.Value
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : Data
    , discussionPropertyIds : Maybe (Array String)
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newIdeaModel : Ideas.New.Types.Model
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


translateNewIdeaMsg : Ideas.New.Types.MsgTranslator Msg
translateNewIdeaMsg =
    Ideas.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewIdeaMsg
        , onIdeaUpserted = ForSelf << IdeaUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewIdeaMsg
        }
