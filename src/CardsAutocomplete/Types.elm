module CardsAutocomplete.Types exposing (..)

import Autocomplete
import Http
import Types exposing (..)


type AutocompleteMenuState
    = AutocompleteMenuHidden
    | AutocompleteMenuSleeping
    | AutocompleteMenuLoading
    | AutocompleteMenuVisible


type ExternalMsg
    = Navigate String


type alias Model =
    { autocomplete : String
    , autocompleteMenuState : AutocompleteMenuState
    , autocompleter : Autocomplete.State
    , autocompletions : List CardAutocompletion
    , selected : Maybe CardAutocompletion
    }


type InternalMsg
    = AutocompleteMsg Autocomplete.Msg
    | Focus
    | HandleEscape
    | InputChanged String
    | KeyboardSelect String
    | LoadMenu
    | MenuLoaded (Result Http.Error CardsAutocompletionBody)
    | MouseClose
    | MouseOpen
    | MouseSelect String
    | NoOp
    | Preview String
    | Reset
    | Wrap Bool


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


autocompleterSize : Int
autocompleterSize =
    5


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg
