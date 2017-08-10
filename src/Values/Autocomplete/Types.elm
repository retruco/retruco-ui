module Values.Autocomplete.Types exposing (..)

import Http
import Types exposing (..)


type AutocompleterState
    = AutocompleterHidden
    | AutocompleterSleeping
    | AutocompleterLoading
    | AutocompleterVisible


type alias Model =
    { autocomplete : String
    , autocompleterState : AutocompleterState
    , autocompletions : List TypedValueAutocompletion
    , selected : Maybe TypedValueAutocompletion
    , valueTypes : List String
    }


type InternalMsg
    = InputChanged String
    | LoadSuggestions
    | NoOp
    | Select (Maybe String)
    | SuggestionsLoaded (Result Http.Error TypedValuesAutocompletionBody)


type Msg
    = ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


autocompleterSize : Int
autocompleterSize =
    5


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg } msg =
    case msg of
        ForSelf internalMsg ->
            onInternalMsg internalMsg
