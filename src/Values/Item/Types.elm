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
    | Retrieve
    | Retrieved (Result Http.Error DataIdBody)



-- | SameKeyPropertiesMsg SameKeyProperties.Types.InternalMsg


type alias Model =
    { argumentsModel : Maybe Arguments.Index.Types.Model
    , authentication : Maybe Authentication
    , data : DataProxy {}
    , httpError : Maybe Http.Error
    , id : String
    , language : I18n.Language

    -- , sameKeyPropertiesModel : Maybe SameKeyProperties.Types.Model
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


valueTypeToTypeLabel : I18n.Language -> ValueType -> String
valueTypeToTypeLabel language valueType =
    I18n.translate language <|
        case valueType of
            BijectiveCardReferenceValue _ ->
                I18n.BijectiveCardReference

            BooleanValue _ ->
                I18n.Boolean

            CardIdArrayValue _ ->
                I18n.CardIdArray

            CardIdValue _ ->
                I18n.CardId

            EmailValue _ ->
                I18n.Email

            ImagePathValue _ ->
                I18n.Image

            LocalizedStringValue _ ->
                I18n.LocalizedString

            NumberValue _ ->
                I18n.Number

            StringValue _ ->
                I18n.String

            UrlValue _ ->
                I18n.Url

            ValueIdArrayValue _ ->
                I18n.ValueIdArray

            ValueIdValue _ ->
                I18n.ValueId

            WrongValue _ schemaId ->
                I18n.UnknownSchemaId schemaId
