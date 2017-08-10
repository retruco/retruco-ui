module Values.Autocomplete.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Dom.Scroll
import Http
import I18n
import Process
import Requests
import Task
import Time exposing (..)
import Types exposing (..)
import Values.Autocomplete.Types exposing (..)


autocompleteToAutocompletion : String -> Model -> Maybe TypedValueAutocompletion
autocompleteToAutocompletion autocomplete model =
    let
        simplified =
            String.toLower <| String.trim autocomplete
    in
        List.filter
            (\autocompletion -> String.toLower autocompletion.autocomplete == simplified)
            model.autocompletions
            |> List.head


idToAutocompletion : String -> Model -> Maybe TypedValueAutocompletion
idToAutocompletion id model =
    List.filter (\autocompletion -> autocompletion.value.id == id) model.autocompletions
        |> List.head


init : List String -> Model
init valueTypes =
    { autocomplete = ""
    , autocompleterState = AutocompleterHidden
    , autocompletions = []
    , selected = Nothing
    , valueTypes = valueTypes
    }


setModelFromSelected : Maybe TypedValueAutocompletion -> Model -> Model
setModelFromSelected selected model =
    let
        autocomplete =
            case selected of
                Just selected ->
                    selected.autocomplete

                Nothing ->
                    model.autocomplete
    in
        { model
            | autocomplete = autocomplete
            , selected = selected
        }


setModelFromTypedValueId : String -> Model -> Model
setModelFromTypedValueId id model =
    setModelFromSelected (idToAutocompletion id model) model


sleepAndThenLoadAutocompleter : Model -> ( Model, Cmd Msg )
sleepAndThenLoadAutocompleter model =
    ( { model | autocompleterState = AutocompleterSleeping }
    , Process.sleep (300 * millisecond)
        |> Task.perform (\() -> (ForSelf <| LoadMenu))
    )


update : InternalMsg -> Maybe Authentication -> I18n.Language -> String -> Model -> ( Model, Cmd Msg )
update msg authentication language fieldId model =
    case msg of
        InputChanged fieldValue ->
            let
                ( newModel, cmd ) =
                    case model.autocompleterState of
                        AutocompleterHidden ->
                            sleepAndThenLoadAutocompleter model

                        AutocompleterSleeping ->
                            ( model, Cmd.none )

                        AutocompleterLoading ->
                            ( { model | autocompleterState = AutocompleterSleeping }, Cmd.none )

                        AutocompleterVisible ->
                            sleepAndThenLoadAutocompleter model
            in
                ( { newModel
                    | autocomplete = fieldValue
                    , selected = autocompleteToAutocompletion fieldValue newModel
                  }
                , cmd
                )

        LoadMenu ->
            ( { model | autocompleterState = AutocompleterLoading }
            , Requests.autocompleteValues
                authentication
                language
                model.valueTypes
                model.autocomplete
                autocompleterSize
                |> Http.send (ForSelf << MenuLoaded)
            )

        MenuLoaded (Err httpError) ->
            case model.autocompleterState of
                AutocompleterSleeping ->
                    ( { model | autocompleterState = AutocompleterLoading }
                    , Requests.autocompleteValues
                        authentication
                        language
                        model.valueTypes
                        model.autocomplete
                        autocompleterSize
                        |> Http.send (ForSelf << MenuLoaded)
                    )

                _ ->
                    ( { model | autocompleterState = AutocompleterHidden }, Cmd.none )

        MenuLoaded (Ok typedValuesAutocompletionBody) ->
            ( { model
                | autocompleterState = AutocompleterVisible
                , autocompletions = typedValuesAutocompletionBody.data
              }
            , Task.attempt
                (\result ->
                    case result of
                        Result.Err err ->
                            Debug.crash ("Dom.Scroll.toTop \"html-element\": " ++ toString err)

                        Result.Ok _ ->
                            ForSelf <| NoOp
                )
                (Dom.Scroll.toBottom "html-element")
            )

        NoOp ->
            ( model, Cmd.none )

        Select id ->
            let
                selected =
                    case id of
                        Just id ->
                            idToAutocompletion id model

                        Nothing ->
                            Nothing
            in
                ( { model | selected = selected }, Cmd.none )
