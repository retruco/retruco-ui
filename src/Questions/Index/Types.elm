module Questions.Index.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Questions.New.Types
import Json.Encode
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = QuestionUpserted Types.DataId
    | NewQuestionMsg Questions.New.Types.InternalMsg
    | PropertyUpserted Json.Encode.Value
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)


type alias Model =
    { authentication : Maybe Authentication
    , data : Data
    , discussionPropertyIds : Maybe (Array String)
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newQuestionModel : Questions.New.Types.Model
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


translateNewQuestionMsg : Questions.New.Types.MsgTranslator Msg
translateNewQuestionMsg =
    Questions.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewQuestionMsg
        , onQuestionUpserted = ForSelf << QuestionUpserted
        , onRequireSignIn = ForParent << RequireSignIn << NewQuestionMsg
        }
