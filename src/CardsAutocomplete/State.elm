module CardsAutocomplete.State exposing (..)

import Autocomplete
import CardsAutocomplete.Types exposing (..)
import Dom
import Http
import Process
import Requests
import Task
import Time exposing (..)
import Types exposing (..)


autocompleteToAutocompletion : String -> Model -> Maybe CardAutocompletion
autocompleteToAutocompletion autocomplete model =
    let
        simplified =
            String.toLower <| String.trim autocomplete
    in
        List.filter
            (\autocompletion -> String.toLower autocompletion.autocomplete == simplified)
            model.autocompletions
            |> List.head


idToAutocompletion : String -> Model -> Maybe CardAutocompletion
idToAutocompletion id model =
    List.filter (\autocompletion -> autocompletion.card.id == id) model.autocompletions
        |> List.head


init : Model
init =
    { autocomplete = ""
    , autocompleteMenuState = AutocompleteMenuHidden
    , autocompleter = Autocomplete.empty
    , autocompletions = []
    , selectedMaybe = Nothing
    }


requestToLoadAutocompleteMenu : Model -> ( Model, Cmd Msg )
requestToLoadAutocompleteMenu model =
    case model.autocompleteMenuState of
        AutocompleteMenuHidden ->
            sleepAndThenLoadAutocompleteMenu model

        AutocompleteMenuSleeping ->
            ( model, Cmd.none )

        AutocompleteMenuLoading ->
            ( { model | autocompleteMenuState = AutocompleteMenuSleeping }, Cmd.none )

        AutocompleteMenuVisible ->
            sleepAndThenLoadAutocompleteMenu model


resetAutocompleteMenu : Model -> Model
resetAutocompleteMenu model =
    { model
        | autocompleter = Autocomplete.empty
        , autocompleteMenuState = AutocompleteMenuHidden
    }


setModelFromCardId : String -> Model -> Model
setModelFromCardId id model =
    setModelFromSelectedMaybe (idToAutocompletion id model) model


setModelFromSelectedMaybe : Maybe CardAutocompletion -> Model -> Model
setModelFromSelectedMaybe selectedMaybe model =
    let
        autocomplete =
            case selectedMaybe of
                Just selected ->
                    selected.autocomplete

                Nothing ->
                    model.autocomplete
    in
        { model
            | autocomplete = autocomplete
            , selectedMaybe = selectedMaybe
        }


sleepAndThenLoadAutocompleteMenu : Model -> ( Model, Cmd Msg )
sleepAndThenLoadAutocompleteMenu model =
    ( { model | autocompleteMenuState = AutocompleteMenuSleeping }
    , Process.sleep (300 * millisecond)
        |> Task.perform (\() -> (ForSelf <| LoadMenu))
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map (ForSelf << AutocompleteMsg) Autocomplete.subscription


update : InternalMsg -> String -> Model -> ( Model, Cmd Msg )
update msg fieldId model =
    case msg of
        AutocompleteMsg childMsg ->
            let
                ( newAutocompleter, newMsgMaybe ) =
                    Autocomplete.update
                        updateAutocompleteConfig
                        childMsg
                        autocompleterSize
                        model.autocompleter
                        model.autocompletions

                newModel =
                    { model | autocompleter = newAutocompleter }
            in
                case newMsgMaybe of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just newMsg ->
                        update newMsg fieldId newModel

        Focus ->
            model ! []

        HandleEscape ->
            ( { model | selectedMaybe = autocompleteToAutocompletion model.autocomplete model }
                |> resetAutocompleteMenu
            , Cmd.none
            )

        KeyboardSelect id ->
            let
                newModel =
                    setModelFromCardId id model
                        |> resetAutocompleteMenu
            in
                newModel ! []

        InputChanged fieldValue ->
            let
                ( newModel, cmd ) =
                    requestToLoadAutocompleteMenu model
            in
                ( { newModel
                    | autocomplete = fieldValue
                    , selectedMaybe = autocompleteToAutocompletion fieldValue newModel
                  }
                , cmd
                )

        LoadMenu ->
            ( { model | autocompleteMenuState = AutocompleteMenuLoading }
            , Requests.autocompleteCards Nothing Nothing model.autocomplete autocompleterSize
                |> Http.send (ForSelf << MenuLoaded)
            )

        MenuLoaded (Err httpError) ->
            let
                _ =
                    Debug.log "CardsAutocomplete.State update MenuLoaded Err" httpError
            in
                case model.autocompleteMenuState of
                    AutocompleteMenuSleeping ->
                        ( { model | autocompleteMenuState = AutocompleteMenuLoading }
                        , Requests.autocompleteCards Nothing Nothing model.autocomplete autocompleterSize
                            |> Http.send (ForSelf << MenuLoaded)
                        )

                    _ ->
                        ( { model | autocompleteMenuState = AutocompleteMenuHidden }, Cmd.none )

        MenuLoaded (Ok cardsAutocompletionBody) ->
            ( { model
                | autocompleteMenuState = AutocompleteMenuVisible
                , autocompletions = cardsAutocompletionBody.data
              }
            , Cmd.none
            )

        MouseClose ->
            let
                prefix =
                    if String.isEmpty fieldId then
                        fieldId
                    else
                        fieldId ++ "."
            in
                ( { model
                    | autocomplete =
                        case model.selectedMaybe of
                            Just selected ->
                                selected.autocomplete

                            Nothing ->
                                model.autocomplete
                  }
                    |> resetAutocompleteMenu
                , Task.attempt (\_ -> ForSelf NoOp) (Dom.focus (prefix ++ "autocomplete"))
                )

        MouseOpen ->
            requestToLoadAutocompleteMenu model

        MouseSelect id ->
            let
                prefix =
                    if String.isEmpty fieldId then
                        fieldId
                    else
                        fieldId ++ "."

                newModel =
                    setModelFromCardId id model
                        |> resetAutocompleteMenu
            in
                ( newModel, Task.attempt (\_ -> ForSelf NoOp) (Dom.focus (prefix ++ "autocomplete")) )

        NoOp ->
            model ! []

        Preview id ->
            ( { model | selectedMaybe = idToAutocompletion id model }
            , Cmd.none
            )

        Reset ->
            ( { model
                | autocompleter = Autocomplete.reset updateAutocompleteConfig model.autocompleter
                , selectedMaybe = autocompleteToAutocompletion model.autocomplete model
              }
            , Cmd.none
            )

        Wrap toTop ->
            case model.selectedMaybe of
                Just selected ->
                    update Reset fieldId model

                Nothing ->
                    let
                        ( autocompleter, selectedMaybe ) =
                            if toTop then
                                ( Autocomplete.resetToLastItem
                                    updateAutocompleteConfig
                                    model.autocompletions
                                    autocompleterSize
                                    model.autocompleter
                                , List.head <| List.reverse <| model.autocompletions
                                )
                            else
                                ( Autocomplete.resetToFirstItem
                                    updateAutocompleteConfig
                                    model.autocompletions
                                    autocompleterSize
                                    model.autocompleter
                                , List.head <| model.autocompletions
                                )
                    in
                        ( { model
                            | autocompleter = autocompleter
                            , selectedMaybe = selectedMaybe
                          }
                        , Cmd.none
                        )


updateAutocompleteConfig : Autocomplete.UpdateConfig InternalMsg CardAutocompletion
updateAutocompleteConfig =
    { getItemId = .card >> .id
    , onFocus = \id -> Just <| Preview id
    , onKeyDown =
        \code maybeId ->
            if code == 38 || code == 40 then
                Maybe.map Preview maybeId
            else if code == 13 then
                Maybe.map KeyboardSelect maybeId
            else
                Just <| Reset
    , onMouseEnter = \id -> Just <| Preview id
    , onMouseClick = \id -> Just <| MouseSelect id
    , onMouseLeave = \_ -> Just <| Preview ""
    , onTooHigh = Just <| Wrap True
    , onTooLow = Just <| Wrap False
    , separateSelections = False
    }
