module Situations.Item.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Situations.NewSuggestion.Types
import Http
import I18n
import Json.Encode
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = NewSuggestionMsg Situations.NewSuggestion.Types.InternalMsg
    | PropertyUpserted Json.Encode.Value
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | SuggestionUpserted Types.DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : Data
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newSuggestionModel : Situations.NewSuggestion.Types.Model
    , objectId : String
    , showTrashed : Bool
    , situationPropertyIds : Maybe (Array String)
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


translateNewSuggestionMsg : Situations.NewSuggestion.Types.MsgTranslator Msg
translateNewSuggestionMsg =
    Situations.NewSuggestion.Types.translateMsg
        { onInternalMsg = ForSelf << NewSuggestionMsg
        , onRequireSignIn = ForParent << RequireSignIn << NewSuggestionMsg
        , onSuggestionUpserted = ForSelf << SuggestionUpserted
        }
