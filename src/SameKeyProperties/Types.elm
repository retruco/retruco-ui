module SameKeyProperties.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)
import Values.New.Types


type ExternalMsg
    = Navigate String


type InternalMsg
    = NewValueMsg Values.New.Types.InternalMsg
    | RatingPosted (Result Http.Error DataIdBody)
    | Retrieve
    | Retrieved (Result Http.Error DataIdsBody)
    | Upserted (Result Http.Error DataIdBody)
    | ValueUpserted Types.DataId
    | VotePropertyDown String
    | VotePropertyUp String


type alias Model =
    { authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , keyId : String
    , language : I18n.Language
    , newValueModel : Values.New.Types.Model
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


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewValueMsg
        , onValueUpserted = ForSelf << ValueUpserted
        }
