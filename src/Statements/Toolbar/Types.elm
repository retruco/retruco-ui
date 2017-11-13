module Statements.Toolbar.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = DataUpdated Data
    | Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = Rate (Maybe Int)
    | RatingPosted (Result Http.Error DataIdBody)
    | ShareOnFacebook String
    | ShareOnGooglePlus String
    | ShareOnLinkedIn String
    | ShareOnTwitter String
    | Start
    | StatementRetrieved (Result Http.Error DataIdBody)
    | Trash TrashAction
    | TrashPosted (Result Http.Error DataIdBody)
    | TrashRatingPosted (Result Http.Error DataIdBody)
    | TrashRetrieved (Result Http.Error DataIdsBody)


type alias Model statementType =
    { authentication : Maybe Authentication
    , data : Data
    , embed : Bool
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , statement : statementType
    , trashAction : Maybe TrashAction
    , trashPropertyId : Maybe String
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onDataUpdated : Data -> parentMsg
    , onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


type TrashAction
    = DebateTrash
    | RateTrash (Maybe Int)


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
