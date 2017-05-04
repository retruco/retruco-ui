module Concepts.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Types exposing (..)
import Values.New.Types


type ExternalMsg
    = ConceptUpserted DataId


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = NewValueMsg Values.New.Types.InternalMsg
    | Submit
    | Rated (Result Http.Error DataIdBody)
    | Upserted DataId


type alias Model =
    { authentication : Maybe Authentication
    , data : DataId
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , newValueModel : Values.New.Types.Model
    , validFieldTypes : List String
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onConceptUpserted : DataId -> parentMsg
    , onInternalMsg : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onConceptUpserted, onInternalMsg } msg =
    case msg of
        ForParent (ConceptUpserted data) ->
            onConceptUpserted data

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewValueMsg
        , onValueUpserted = ForSelf << Upserted
        }
