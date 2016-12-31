module Card.Types exposing (..)

import Http
import I18n
import Types exposing (..)
import WebData exposing (..)


type ExternalMsg
    = Navigate String


type InternalMsg
    = Retrieve
    | Retrieved (Result Http.Error DataIdBody)


type alias Model =
    { id : String
    , language : I18n.Language
    , webData : WebData DataIdBody
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
