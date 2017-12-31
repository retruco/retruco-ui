module Proposals.Index.Types exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String


type alias FormErrors =
    Dict String String


type InternalMsg
    = Retrieve Int
    | Retrieved (Result Http.Error DataWithIdsBody)
    | SearchSortChanged String
    | SearchTermChanged String
    | Submit


type alias Model =
    { authentication : Maybe Authentication
    , count : Int
    , data : Data
    , embed : Bool
    , errors : FormErrors
    , httpError : Maybe Http.Error
    , ids : Maybe (Array String)
    , language : I18n.Language
    , searchCriteria : SearchCriteria
    , searchSort : String
    , searchTerm : String
    , showTrashed : Bool
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


type alias SearchCriteria =
    { sort : String
    , term : Maybe String
    }


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg
