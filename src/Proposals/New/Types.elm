module Affirmations.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Types exposing (..)
import Values.New.Types


type ExternalMsg
    = AffirmationUpserted DataId
    | RequireSignIn InternalMsg


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
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onAffirmationUpserted : DataId -> parentMsg
    , onInternalMsg : InternalMsg -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onAffirmationUpserted, onInternalMsg, onRequireSignIn } msg =
    case msg of
        ForParent (AffirmationUpserted data) ->
            onAffirmationUpserted data

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = ForSelf << NewValueMsg
        , onRequireSignIn =
            \newValueMsg ->
                if newValueMsg == Values.New.Types.Submit then
                    ForParent <| RequireSignIn Submit
                else
                    ForParent <| RequireSignIn <| NewValueMsg newValueMsg
        , onValueUpserted = ForSelf << Upserted
        }
