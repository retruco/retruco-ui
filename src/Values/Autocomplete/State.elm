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


init : List String -> List String -> Model
init schemaIds widgetIds =
    { autocomplete = ""
    , autocompleterState = AutocompleterHidden
    , autocompletions = []
    , schemaIds = schemaIds
    , selected = Nothing
    , widgetIds = widgetIds
    }


setSchemaIdsAndWidgetIds : List String -> List String -> Model -> Model
setSchemaIdsAndWidgetIds schemaIds widgetIds model =
    { model
        | autocompletions = []
        , schemaIds = schemaIds
        , widgetIds = widgetIds
    }


sleepAndThenLoadAutocompleter : Model -> ( Model, Cmd Msg )
sleepAndThenLoadAutocompleter model =
    ( { model | autocompleterState = AutocompleterSleeping }
    , Process.sleep (300 * millisecond)
        |> Task.perform (\() -> (ForSelf <| LoadSuggestions))
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

        LoadSuggestions ->
            ( { model | autocompleterState = AutocompleterLoading }
            , Requests.autocompleteValues
                authentication
                language
                model.schemaIds
                model.widgetIds
                model.autocomplete
                autocompleterSize
                |> Http.send (ForSelf << SuggestionsLoaded)
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

        SuggestionsLoaded (Err httpError) ->
            case model.autocompleterState of
                AutocompleterSleeping ->
                    ( { model | autocompleterState = AutocompleterLoading }
                    , Requests.autocompleteValues
                        authentication
                        language
                        model.schemaIds
                        model.widgetIds
                        model.autocomplete
                        autocompleterSize
                        |> Http.send (ForSelf << SuggestionsLoaded)
                    )

                _ ->
                    ( { model | autocompleterState = AutocompleterHidden }, Cmd.none )

        SuggestionsLoaded (Ok typedValuesAutocompletionBody) ->
            ( { model
                | autocompleterState = AutocompleterVisible
                , autocompletions = typedValuesAutocompletionBody.data
              }
            , Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            Debug.crash ("Dom.Scroll.toTop \"html-element\": " ++ toString err)

                        Ok _ ->
                            ForSelf <| NoOp
                )
                (Dom.Scroll.toBottom "html-element")
            )
