module Person.Embed exposing (..)

import Autocomplete
import Basics.Extra exposing (never)
import Dict
import Dom
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (keyCode, onFocus, onInput, onWithOptions)
import Http
import Json.Decode
import Process
import Requests exposing (..)
import String
import Task
import Time exposing (millisecond)
import Types exposing (..)


type alias Model =
    StatementEmbed


type Msg
    = AutocompleteMsg Autocomplete.Msg
    | Focus
    | HandleEscape
    | InputChanged String
    | KeyboardSelect String
    | LoadMenu
    | LoadMenuErr Http.Error
    | LoadMenuOk StatementsAutocompletionBody
    | MouseSelect String
    | NoOp
    | Preview String
    | Reset
    | Wrap Bool


autocompleterSize : Int
autocompleterSize =
    5


acceptablePeople : String -> List StatementAutocompletion -> List StatementAutocompletion
acceptablePeople query people =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .autocomplete) people


getAutocompletionFromId : String -> Model -> Maybe StatementAutocompletion
getAutocompletionFromId id model =
    List.filter (\autocompletion -> autocompletion.statement.id == id) model.autocompletions
        |> List.head


init : Model
init =
    initStatementEmbed


resetAutocompleteMenu : Model -> Model
resetAutocompleteMenu model =
    { model
        | autocompleter = Autocomplete.empty
        , autocompleteMenuState = AutocompleteMenuHidden
    }


setModelFromSelectedMaybe : Maybe StatementAutocompletion -> Model -> Model
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


setModelFromStatementId : String -> Model -> Model
setModelFromStatementId id model =
    setModelFromSelectedMaybe (getAutocompletionFromId id model) model


sleepAndThenLoadAutocompleteMenu : Model -> ( Model, Cmd Msg )
sleepAndThenLoadAutocompleteMenu model =
    ( { model | autocompleteMenuState = AutocompleteMenuSleeping }
    , Process.sleep (300 * millisecond)
        |> Task.perform never (\() -> LoadMenu)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AutocompleteMsg Autocomplete.subscription


update : Msg -> String -> Model -> ( Model, Cmd Msg )
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
            ( { model | selectedMaybe = Nothing }
                |> resetAutocompleteMenu
            , Cmd.none
            )

        KeyboardSelect id ->
            let
                newModel =
                    setModelFromStatementId id model
                        |> resetAutocompleteMenu
            in
                newModel ! []

        InputChanged fieldValue ->
            let
                ( newModel, cmd ) =
                    case model.autocompleteMenuState of
                        AutocompleteMenuHidden ->
                            sleepAndThenLoadAutocompleteMenu model

                        AutocompleteMenuSleeping ->
                            ( model, Cmd.none )

                        AutocompleteMenuLoading ->
                            ( { model | autocompleteMenuState = AutocompleteMenuSleeping }, Cmd.none )

                        AutocompleteMenuVisible ->
                            sleepAndThenLoadAutocompleteMenu model
            in
                ( { newModel
                    | autocomplete = fieldValue
                    , selectedMaybe = Nothing
                  }
                , cmd
                )

        LoadMenu ->
            ( { model | autocompleteMenuState = AutocompleteMenuLoading }
            , Task.perform
                LoadMenuErr
                LoadMenuOk
                (newTaskAutocompleteStatements Nothing "Person" model.autocomplete autocompleterSize)
            )

        LoadMenuErr err ->
            let
                _ =
                    Debug.log "PersonEmbed LoadMenuErr" err
            in
                case model.autocompleteMenuState of
                    AutocompleteMenuSleeping ->
                        ( { model | autocompleteMenuState = AutocompleteMenuLoading }
                        , Task.perform
                            LoadMenuErr
                            LoadMenuOk
                            (newTaskAutocompleteStatements Nothing "Person" model.autocomplete autocompleterSize)
                        )

                    _ ->
                        ( { model | autocompleteMenuState = AutocompleteMenuHidden }, Cmd.none )

        LoadMenuOk statementsAutocompletionBody ->
            ( { model
                | autocompleteMenuState = AutocompleteMenuVisible
                , autocompletions = statementsAutocompletionBody.data
              }
            , Cmd.none
            )

        MouseSelect id ->
            let
                prefix =
                    if String.isEmpty fieldId then
                        fieldId
                    else
                        fieldId ++ "."

                newModel =
                    setModelFromStatementId id model
                        |> resetAutocompleteMenu
            in
                ( newModel, Task.perform (\err -> NoOp) (\_ -> NoOp) (Dom.focus (prefix ++ "autocomplete")) )

        NoOp ->
            model ! []

        Preview id ->
            ( { model | selectedMaybe = getAutocompletionFromId id model }
            , Cmd.none
            )

        Reset ->
            ( { model
                | autocompleter = Autocomplete.reset updateAutocompleteConfig model.autocompleter
                , selectedMaybe = Nothing
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


updateAutocompleteConfig : Autocomplete.UpdateConfig Msg StatementAutocompletion
updateAutocompleteConfig =
    { onFocus = \id -> Just <| Preview id
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
    , toId = .statement >> .id
    }


view : String -> String -> Model -> FormErrors -> (Msg -> parentMsg) -> Html parentMsg
view fieldLabel fieldId model errors changed =
    let
        prefix =
            if String.isEmpty fieldId then
                fieldId
            else
                fieldId ++ "."
    in
        fieldset [ class "form-group" ]
            [ legend [] [ text fieldLabel ]
            , Html.App.map changed (viewAutocomplete fieldId model errors)
            ]


viewAutocompleteItemContent : StatementAutocompletion -> List (Html Never)
viewAutocompleteItemContent statementAutocompletion =
    [ Html.text statementAutocompletion.autocomplete ]


viewAutocomplete : String -> Model -> FormErrors -> Html Msg
viewAutocomplete parentId model errors =
    let
        decodeKeyCode =
            (Json.Decode.customDecoder keyCode
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
            )

        query =
            case model.selectedMaybe of
                Just selected ->
                    selected.autocomplete

                Nothing ->
                    model.autocomplete

        fieldId =
            if String.isEmpty parentId then
                "autocomplete"
            else
                parentId ++ ".autocomplete"

        errorId =
            fieldId ++ "-error"

        errorMaybe =
            (Dict.get fieldId errors)

        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-error"
                    , [ ariaDescribedby errorId ]
                    , [ span
                            [ class "help-block"
                            , id errorId
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )

        showAutocompleteMenu =
            case model.autocompleteMenuState of
                AutocompleteMenuVisible ->
                    True

                _ ->
                    False

        menu =
            if showAutocompleteMenu then
                [ Html.App.map AutocompleteMsg
                    (Autocomplete.view
                        { toId = .statement >> .id
                        , viewItemContent = viewAutocompleteItemContent
                        }
                        autocompleterSize
                        model.autocompleter
                        model.autocompletions
                    )
                ]
            else
                []
    in
        div [ class ("form-group" ++ errorClass) ]
            (List.concat
                [ [ label [ class "control-label", for fieldId ]
                        [ span [ class "fa fa-search" ] []
                        , text " "
                        , text "Search"
                        ]
                  , div [ class "input-group" ]
                        [ input
                            (List.concat
                                [ [ attribute "aria-autocomplete" "list"
                                  , ariaExpanded <| String.toLower <| toString showAutocompleteMenu
                                  , attribute "aria-haspopup" <| String.toLower <| toString showAutocompleteMenu
                                  , attribute "aria-owns" "list-of-presidents"
                                  , autocomplete False
                                  , class "form-control"
                                  , id fieldId
                                  , onFocus Focus
                                  , onInput InputChanged
                                  , onWithOptions "keydown" { preventDefault = True, stopPropagation = False } decodeKeyCode
                                  , placeholder "John Doe (@JohnDoe)"
                                  , attribute "role" "combobox"
                                  , type' "text"
                                  , value query
                                  ]
                                , (case model.selectedMaybe of
                                    Just selected ->
                                        [ ariaActiveDescendant selected.autocomplete ]

                                    Nothing ->
                                        []
                                  )
                                , errorAttributes
                                ]
                            )
                            []
                        , (case model.selectedMaybe of
                            Just selected ->
                                span [ class "input-group-addon" ]
                                    [ span
                                        [ ariaHidden True
                                        , class "text-success fa fa-check fa-fw"
                                        ]
                                        []
                                    , span
                                        [ class "sr-only" ]
                                        [ text "Person found" ]
                                    ]

                            Nothing ->
                                (case model.autocompleteMenuState of
                                    AutocompleteMenuHidden ->
                                        span [ class "input-group-addon" ]
                                            [ span
                                                [ ariaHidden True
                                                , class "fa fa-caret-down fa-fw"
                                                ]
                                                []
                                            , span
                                                [ class "sr-only" ]
                                                [ text "Type some characters to find a person" ]
                                            ]

                                    AutocompleteMenuVisible ->
                                        span [ class "bg-inverse text-white input-group-addon" ]
                                            [ span
                                                [ ariaHidden True
                                                , class "fa fa-caret-down fa-fw"
                                                ]
                                                []
                                            , span
                                                [ class "sr-only" ]
                                                [ text "Select a person or type more characters" ]
                                            ]

                                    _ ->
                                        span [ class "input-group-addon" ]
                                            [ span [ ariaHidden True, class "fa fa-fw fa-refresh fa-spin" ] []
                                            , span [ class "sr-only" ] [ text "Loading menu..." ]
                                            ]
                                )
                          )
                        , span [ class "input-group-btn" ]
                            [ button
                                [ class "btn btn-primary"
                                , disabled
                                    (case model.selectedMaybe of
                                        Just selected ->
                                            False

                                        Nothing ->
                                            True
                                    )
                                ]
                                [ text "Show" ]
                            ]
                        , span [ class "input-group-btn" ]
                            [ button
                                [ class "btn btn-primary" ]
                                [ text "New" ]
                            ]
                        ]
                  , span [ class "sr-only" ] [ text "Loading menu..." ]
                  ]
                , menu
                , errorBlock
                ]
            )
