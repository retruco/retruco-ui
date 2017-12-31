module Cards.New.Types exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Types exposing (..)
import Values.Autocomplete.Types


type ExternalMsg
    = CardUpserted DataWithId
    | RequireSignIn InternalMsg


type alias FormErrors =
    Dict String I18n.TranslationId


type InternalMsg
    = LanguageChanged String
    | NameChanged String
    | NameLocalizationPropertyUpserted (Result Http.Error DataWithIdBody)
    | NamePropertyUpserted (Result Http.Error DataWithIdBody)
    | NamesAutocompleteMsg Values.Autocomplete.Types.InternalMsg
    | NameUpserted (Result Http.Error DataWithIdBody)
    | Submit
    | Upserted (Result Http.Error DataWithIdBody)


type alias Model =
    { authentication : Maybe Authentication
    , cardId : String
    , data : DataWithId
    , embed : Bool
    , errors : FormErrors
    , httpError : Maybe Http.Error
    , language : I18n.Language
    , languageId : String
    , name : String
    , nameField : Maybe Field
    , nameId : String
    , namesAutocompleteModel : Values.Autocomplete.Types.Model
    }


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onRequireSignIn : InternalMsg -> parentMsg
    , onCardUpserted : DataWithId -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onRequireSignIn, onCardUpserted } msg =
    case msg of
        ForParent (CardUpserted data) ->
            onCardUpserted data

        ForParent (RequireSignIn completionMsg) ->
            onRequireSignIn completionMsg

        ForSelf internalMsg ->
            onInternalMsg internalMsg


translateNamesAutocompleteMsg : Values.Autocomplete.Types.MsgTranslator Msg
translateNamesAutocompleteMsg =
    Values.Autocomplete.Types.translateMsg
        { onInternalMsg = ForSelf << NamesAutocompleteMsg
        }
