module Arguments.Index.Types exposing (..)

import Arguments.New.Types
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String


type InternalMsg
    = NewArgumentMsg Arguments.New.Types.InternalMsg
    | RatingPosted (Result Http.Error DataIdBody)
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | Upserted Types.DataId
    | VotePropertyDown String
    | VotePropertyUp String


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newArgumentModel : Arguments.New.Types.Model
    , objectId : String
    , propertyIds : Maybe (List String)
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewArgumentMsg : Arguments.New.Types.MsgTranslator Msg
translateNewArgumentMsg =
    Arguments.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewArgumentMsg
        , onPropertyUpserted = ForSelf << Upserted
        }
