module Person.Embed exposing (..)

import Autocomplete
import Dict
import Dom
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (keyCode, onFocus, onInput, onWithOptions)
import Json.Decode
import String
import Task
import Types exposing (..)


type alias Model =
    PersonEmbed


type Msg
    = AutocompleteChanged String
    | AutocompleteFocus
    | AutocompleteMsg Autocomplete.Msg
      --
    | HandleEscape
    | NoOp
    | PreviewPerson String
    | Reset
    | SelectPersonKeyboard String
    | SelectPersonMouse String
    | Wrap Bool


autocompleterSize : Int
autocompleterSize =
    5


presidents : List PersonAutocompletion
presidents =
    [ PersonAutocompletion "George Washington" "1732" "Westmoreland County" "Virginia"
    , PersonAutocompletion "John Adams" "1735" "Braintree" "Massachusetts"
    , PersonAutocompletion "Thomas Jefferson" "1743" "Shadwell" "Virginia"
    , PersonAutocompletion "James Madison" "1751" "Port Conway" "Virginia"
    , PersonAutocompletion "James Monroe" "1758" "Monroe Hall" "Virginia"
    , PersonAutocompletion "Andrew Jackson" "1767" "Waxhaws Region" "South/North Carolina"
    , PersonAutocompletion "John Quincy Adams" "1768" "Braintree" "Massachusetts"
    , PersonAutocompletion "William Henry Harrison" "1773" "Charles City County" "Virginia"
    , PersonAutocompletion "Martin Van Buren" "1782" "Kinderhook" "New York"
    , PersonAutocompletion "Zachary Taylor" "1784" "Barboursville" "Virginia"
    , PersonAutocompletion "John Tyler" "1790" "Charles City County" "Virginia"
    , PersonAutocompletion "James Buchanan" "1791" "Cove Gap" "Pennsylvania"
    , PersonAutocompletion "James K. Polk" "1795" "Pineville" "North Carolina"
    , PersonAutocompletion "Millard Fillmore" "1800" "Summerhill" "New York"
    , PersonAutocompletion "Franklin Pierce" "1804" "Hillsborough" "New Hampshire"
    , PersonAutocompletion "Andrew Johnson" "1808" "Raleigh" "North Carolina"
    , PersonAutocompletion "Abraham Lincoln" "1809" "Sinking spring" "Kentucky"
    , PersonAutocompletion "Ulysses S. Grant" "1822" "Point Pleasant" "Ohio"
    , PersonAutocompletion "Rutherford B. Hayes" "1823" "Delaware" "Ohio"
    , PersonAutocompletion "Chester A. Arthur" "1829" "Fairfield" "Vermont"
    , PersonAutocompletion "James A. Garfield" "1831" "Moreland Hills" "Ohio"
    , PersonAutocompletion "Benjamin Harrison" "1833" "North Bend" "Ohio"
    , PersonAutocompletion "Grover Cleveland" "1837" "Caldwell" "New Jersey"
    , PersonAutocompletion "William McKinley" "1843" "Niles" "Ohio"
    , PersonAutocompletion "Woodrow Wilson" "1856" "Staunton" "Virginia"
    , PersonAutocompletion "William Howard Taft" "1857" "Cincinnati" "Ohio"
    , PersonAutocompletion "Theodore Roosevelt" "1858" "New York City" "New York"
    , PersonAutocompletion "Warren G. Harding" "1865" "Blooming Grove" "Ohio"
    , PersonAutocompletion "Calvin Coolidge" "1872" "Plymouth" "Vermont"
    , PersonAutocompletion "Herbert Hoover" "1874" "West Branch" "Iowa"
    , PersonAutocompletion "Franklin D. Roosevelt" "1882" "Hyde Park" "New York"
    , PersonAutocompletion "Harry S. Truman" "1884" "Lamar" "Missouri"
    , PersonAutocompletion "Dwight D. Eisenhower" "1890" "Denison" "Texas"
    , PersonAutocompletion "Lyndon B. Johnson" "1908" "Stonewall" "Texas"
    , PersonAutocompletion "Ronald Reagan" "1911" "Tampico" "Illinois"
    , PersonAutocompletion "Richard M. Nixon" "1913" "Yorba Linda" "California"
    , PersonAutocompletion "Gerald R. Ford" "1914" "Omaha" "Nebraska"
    , PersonAutocompletion "John F. Kennedy" "1917" "Brookline" "Massachusetts"
    , PersonAutocompletion "George H. W. Bush" "1924" "Milton" "Massachusetts"
    , PersonAutocompletion "Jimmy Carter" "1925" "Plains" "Georgia"
    , PersonAutocompletion "George W. Bush" "1946" "New Haven" "Connecticut"
    , PersonAutocompletion "Bill Clinton" "1946" "Hope" "Arkansas"
    , PersonAutocompletion "Barack Obama" "1961" "Honolulu" "Hawaii"
    ]


acceptablePeople : String -> List PersonAutocompletion -> List PersonAutocompletion
acceptablePeople query people =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .autocomplete) people


getPersonAtId : List PersonAutocompletion -> String -> Maybe PersonAutocompletion
getPersonAtId people id =
    List.filter (\person -> person.id == id) people
        |> List.head


init : Model
init =
    initPersonEmbed


resetAutocompleteMenu : Model -> Model
resetAutocompleteMenu model =
    { model
        | autocompleter = Autocomplete.empty
        , showAutocompleteMenu = False
    }


setModelFromSelectedMaybe : Maybe PersonAutocompletion -> Model -> Model
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


setModelFromPersonId : String -> Model -> Model
setModelFromPersonId id model =
    setModelFromSelectedMaybe (getPersonAtId presidents id) model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map AutocompleteMsg Autocomplete.subscription


updateAutocompleteConfig : Autocomplete.UpdateConfig Msg PersonAutocompletion
updateAutocompleteConfig =
    { onFocus = \id -> Just <| PreviewPerson id
    , onKeyDown =
        \code maybeId ->
            if code == 38 || code == 40 then
                Maybe.map PreviewPerson maybeId
            else if code == 13 then
                Maybe.map SelectPersonKeyboard maybeId
            else
                Just <| Reset
    , onMouseEnter = \id -> Just <| PreviewPerson id
    , onMouseClick = \id -> Just <| SelectPersonMouse id
    , onMouseLeave = \_ -> Just <| PreviewPerson ""
    , onTooHigh = Just <| Wrap True
    , onTooLow = Just <| Wrap False
    , separateSelections = False
    , toId = .id
    }


update : Msg -> String -> Model -> ( Model, Cmd Msg )
update msg fieldId model =
    case (Debug.log "Person.Embed.msg" msg) of
        AutocompleteChanged fieldValue ->
            let
                showAutocompleteMenu =
                    not << List.isEmpty <| (acceptablePeople fieldValue presidents)
            in
                ( { model
                    | autocomplete = fieldValue
                    , selectedMaybe = Nothing
                    , showAutocompleteMenu = showAutocompleteMenu
                  }
                , Cmd.none
                )

        AutocompleteMsg childMsg ->
            let
                ( newAutocompleter, newMsgMaybe ) =
                    Autocomplete.update
                        updateAutocompleteConfig
                        childMsg
                        autocompleterSize
                        model.autocompleter
                        (acceptablePeople model.autocomplete presidents)

                newModel =
                    { model | autocompleter = newAutocompleter }
            in
                case newMsgMaybe of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just newMsg ->
                        update newMsg fieldId newModel

        AutocompleteFocus ->
            model ! []

        HandleEscape ->
            ( { model | selectedMaybe = Nothing }
                |> resetAutocompleteMenu
            , Cmd.none
            )

        NoOp ->
            model ! []

        PreviewPerson id ->
            ( { model | selectedMaybe = getPersonAtId presidents id }
            , Cmd.none
            )

        Reset ->
            ( { model
                | autocompleter = Autocomplete.reset updateAutocompleteConfig model.autocompleter
                , selectedMaybe = Nothing
              }
            , Cmd.none
            )

        SelectPersonKeyboard id ->
            let
                newModel =
                    setModelFromPersonId id model
                        |> resetAutocompleteMenu
            in
                newModel ! []

        SelectPersonMouse id ->
            let
                prefix =
                    if String.isEmpty fieldId then
                        fieldId
                    else
                        fieldId ++ "."

                newModel =
                    setModelFromPersonId id model
                        |> resetAutocompleteMenu
            in
                ( newModel, Task.perform (\err -> NoOp) (\_ -> NoOp) (Dom.focus (prefix ++ "autocomplete")) )

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
                                    (acceptablePeople model.autocomplete presidents)
                                    autocompleterSize
                                    model.autocompleter
                                , List.head <|
                                    List.reverse <|
                                        List.take autocompleterSize <|
                                            acceptablePeople model.autocomplete presidents
                                )
                            else
                                ( Autocomplete.resetToFirstItem
                                    updateAutocompleteConfig
                                    (acceptablePeople model.autocomplete presidents)
                                    autocompleterSize
                                    model.autocompleter
                                , List.head <|
                                    List.take autocompleterSize <|
                                        acceptablePeople model.autocomplete presidents
                                )
                    in
                        ( { model
                            | autocompleter = autocompleter
                            , selectedMaybe = selectedMaybe
                          }
                        , Cmd.none
                        )


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
            , button
                [ class "btn btn-primary", type' "submit" ]
                [ text "Show" ]
            , button
                [ class "btn btn-primary", type' "submit" ]
                [ text "New" ]
            ]


viewAutocompleteItemContent : PersonAutocompletion -> List (Html Never)
viewAutocompleteItemContent personAutocompletion =
    [ Html.text personAutocompletion.autocomplete ]


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

        menu =
            if model.showAutocompleteMenu then
                [ Html.App.map AutocompleteMsg
                    (Autocomplete.view
                        { toId = .id
                        , viewItemContent = viewAutocompleteItemContent
                        }
                        autocompleterSize
                        model.autocompleter
                        (acceptablePeople model.autocomplete presidents)
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
                  , input
                        (List.concat
                            [ [ attribute "aria-autocomplete" "list"
                              , ariaExpanded <| String.toLower <| toString model.showAutocompleteMenu
                              , attribute "aria-haspopup" <| String.toLower <| toString model.showAutocompleteMenu
                              , attribute "aria-owns" "list-of-presidents"
                              , autocomplete False
                              , class "form-control"
                              , id fieldId
                              , onFocus AutocompleteFocus
                              , onInput AutocompleteChanged
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
                  ]
                , menu
                , errorBlock
                ]
            )
