module Values.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.Autocomplete.Types
import Dict exposing (Dict)
import Http
import I18n
import Image.Types exposing (..)
import Ports
import Types exposing (..)
import Values.Autocomplete.Types


type ExternalMsg
    = RequireSignIn InternalMsg
    | ValueUpserted DataWithId


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = CardsAutocompleteMsg Cards.Autocomplete.Types.InternalMsg
    | FieldTypeChanged String
    | ImageRead Ports.ImagePortData
    | ImageSelected
    | ImageUploaded (Result Http.Error String)
    | LanguageChanged String
    | LocalizationPropertyUpserted (Result Http.Error DataWithIdBody)
    | ObjectUpserted DataWithId ObjectWrapper
    | Submit
    | Upserted (Result Http.Error DataWithIdBody)
    | ValueChanged String
    | ValueChecked Bool
    | ValuesAutocompleteMsg Values.Autocomplete.Types.InternalMsg


type alias Model =
    { authentication : Maybe Authentication
    , booleanValue : Bool
    , cardsAutocompleteModel : Cards.Autocomplete.Types.Model
    , embed : Bool
    , errors : FormErrors
    , field : Maybe Field
    , fieldType : String
    , httpError : Maybe Http.Error
    , imageUploadStatus : ImageUploadStatus
    , language : I18n.Language
    , languageId : String
    , validFieldTypes : List String
    , value : String
    , valuesAutocompleteModel : Values.Autocomplete.Types.Model
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    , onValueUpserted : DataWithId -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateCardsAutocompleteMsg : Cards.Autocomplete.Types.MsgTranslator Msg
translateCardsAutocompleteMsg =
    Cards.Autocomplete.Types.translateMsg
        { onInternalMsg = ForSelf << CardsAutocompleteMsg
        }


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onRequireSignIn, onValueUpserted } msg =
    case msg of
        ForParent (ValueUpserted data) ->
            onValueUpserted data

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateValuesAutocompleteMsg : Values.Autocomplete.Types.MsgTranslator Msg
translateValuesAutocompleteMsg =
    Values.Autocomplete.Types.translateMsg
        { onInternalMsg = ForSelf << ValuesAutocompleteMsg
        }
