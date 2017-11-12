module Discussions.Item.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Ideas.Index.Types
import Interventions.Index.Types
import Questions.Index.Types
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = IdeasMsg Ideas.Index.Types.InternalMsg
    | InterventionsMsg Interventions.Index.Types.InternalMsg
    | QuestionsMsg Questions.Index.Types.InternalMsg



-- | TrashMsg Trash.Index.Types.InternalMsg


type alias Model =
    { activeTab : Tab
    , authentication : Maybe Authentication
    , data : Data
    , language : I18n.Language
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


type Tab
    = IdeasTab Ideas.Index.Types.Model
    | InterventionsTab Interventions.Index.Types.Model
    | NoTab
    | QuestionsTab Questions.Index.Types.Model



-- | TrashTab Trash.Index.Types.Model


translateIdeasMsg : Ideas.Index.Types.MsgTranslator Msg
translateIdeasMsg =
    Ideas.Index.Types.translateMsg
        { onInternalMsg = ForSelf << IdeasMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << IdeasMsg
        }


translateInterventionsMsg : Interventions.Index.Types.MsgTranslator Msg
translateInterventionsMsg =
    Interventions.Index.Types.translateMsg
        { onInternalMsg = ForSelf << InterventionsMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << InterventionsMsg
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


translateQuestionsMsg : Questions.Index.Types.MsgTranslator Msg
translateQuestionsMsg =
    Questions.Index.Types.translateMsg
        { onInternalMsg = ForSelf << QuestionsMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << QuestionsMsg
        }
