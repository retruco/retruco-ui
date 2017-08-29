module Statements.Toolbar.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = DataUpdated (DataProxy {})
    | Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = Rate (Maybe Int)
    | RatingPosted (Result Http.Error DataIdBody)
    | ShareOnFacebook String
    | ShareOnGooglePlus String
    | ShareOnLinkedIn String
    | ShareOnTwitter String
    | Trash
    | TrashPosted (Result Http.Error DataIdBody)


type alias Model statementType =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , statement : statementType
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onDataUpdated : DataProxy {} -> parentMsg
    , onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onDataUpdated, onInternalMsg, onNavigate, onRequireSignIn } msg =
    case msg of
        ForParent (DataUpdated data) ->
            onDataUpdated data

        ForParent (Navigate path) ->
            onNavigate path

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg
