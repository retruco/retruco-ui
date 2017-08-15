module Values.Item.Types exposing (..)

import Arguments.Index.Types
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Types exposing (..)


type ExternalMsg
    = Navigate String
    | RequireSignIn InternalMsg


type InternalMsg
    = ArgumentsMsg Arguments.Index.Types.InternalMsg
    | DebatePropertiesRetrieved (Result Http.Error DataIdsBody)
    | Retrieve
    | ValueRetrieved (Result Http.Error DataIdBody)



-- | SameKeyPropertiesMsg SameKeyProperties.Types.InternalMsg


type alias Model =
    { argumentsModel : Maybe Arguments.Index.Types.Model
    , authentication : Maybe Authentication
    , data : DataProxy {}
    , debatePropertyIds : Maybe (List String)
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language

    -- , sameKeyPropertiesModel : Maybe SameKeyProperties.Types.Model
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


translateArgumentsMsg : Arguments.Index.Types.MsgTranslator Msg
translateArgumentsMsg =
    Arguments.Index.Types.translateMsg
        { onInternalMsg = ForSelf << ArgumentsMsg
        , onNavigate = ForParent << Navigate
        , onRequireSignIn = ForParent << RequireSignIn << ArgumentsMsg
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



-- translateSameKeyPropertiesMsg : SameKeyProperties.Types.MsgTranslator Msg
-- translateSameKeyPropertiesMsg =
--     SameKeyProperties.Types.translateMsg
--         { onInternalMsg = ForSelf << SameKeyPropertiesMsg
--         , onNavigate = ForParent << Navigate
--         }
