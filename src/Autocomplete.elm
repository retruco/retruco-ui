module Autocomplete
    exposing
        ( view
        , update
        , subscription
        , viewConfig
        , updateConfig
        , State
        , empty
        , reset
        , resetToFirstItem
        , resetToLastItem
        , KeySelected
        , MouseSelected
        , Msg
        , ViewConfig
        , UpdateConfig
        , HtmlDetails
        )

import Char exposing (KeyCode)
import Html exposing (Html, Attribute)
import Html.App
import Html.Attributes
import Html.Keyed
import Html.Events
import Keyboard



-- MODEL


type alias State =
    { key : Maybe String
    , mouse : Maybe String
    }


type alias KeySelected =
    Bool


type alias MouseSelected =
    Bool


empty : State
empty =
    { key = Nothing, mouse = Nothing }


reset : UpdateConfig msg data -> State -> State
reset { separateSelections } { key, mouse } =
    if separateSelections then
        { key = Nothing, mouse = mouse }
    else
        empty


resetToFirstItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToFirstItem config data howManyToShow state =
    resetToFirst config (List.take howManyToShow data) state


resetToFirst : UpdateConfig msg data -> List data -> State -> State
resetToFirst config data state =
    let
        { toId, separateSelections } =
            config

        setFirstItem datum newState =
            { newState | key = Just <| toId <| Debug.log "resetToFirst datum" datum }
    in
        case List.head data of
            Nothing ->
                empty

            Just datum ->
                if separateSelections then
                    reset config state
                        |> setFirstItem datum
                else
                    empty
                        |> setFirstItem datum


resetToLastItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToLastItem config data howManyToShow state =
    let
        reversedData =
            List.reverse <| List.take howManyToShow data
    in
        resetToFirst config reversedData state



-- UPDATE


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub Msg
subscription =
    Keyboard.downs KeyDown


type Msg
    = Focus String
    | KeyDown KeyCode
    | MouseEnter String
    | MouseLeave String
    | MouseClick String
    | NoOp


type alias UpdateConfig msg data =
    { onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onFocus : String -> Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , toId : data -> String
    , separateSelections : Bool
    }


updateConfig :
    { toId : data -> String
    , onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onFocus : String -> Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , separateSelections : Bool
    }
    -> UpdateConfig msg data
updateConfig { toId, onKeyDown, onTooLow, onTooHigh, onFocus, onMouseEnter, onMouseLeave, onMouseClick, separateSelections } =
    { toId = toId
    , onKeyDown = onKeyDown
    , onTooLow = onTooLow
    , onTooHigh = onTooHigh
    , onFocus = onFocus
    , onMouseEnter = onMouseEnter
    , onMouseLeave = onMouseLeave
    , onMouseClick = onMouseClick
    , separateSelections = separateSelections
    }


update : UpdateConfig msg data -> Msg -> Int -> State -> List data -> ( State, Maybe msg )
update config msg howManyToShow state data =
    case msg of
        Focus id ->
            (state, config.onFocus id)
    
        KeyDown keyCode ->
            let
                boundedList =
                    List.map config.toId data
                        |> List.take howManyToShow

                newKey =
                    navigateWithKey keyCode boundedList state.key
            in
                if newKey == state.key && keyCode == 38 then
                    ( state
                    , config.onTooHigh
                    )
                else if newKey == state.key && keyCode == 40 then
                    ( state
                    , config.onTooLow
                    )
                else if config.separateSelections then
                    ( { state | key = newKey }
                    , config.onKeyDown keyCode newKey
                    )
                else
                    ( { key = newKey, mouse = newKey }
                    , config.onKeyDown keyCode newKey
                    )

        MouseEnter id ->
            ( resetMouseStateWithId config.separateSelections id state
            , config.onMouseEnter id
            )

        MouseLeave id ->
            ( resetMouseStateWithId config.separateSelections id state
            , config.onMouseLeave id
            )

        MouseClick id ->
            ( resetMouseStateWithId config.separateSelections id state
            , config.onMouseClick id
            )

        NoOp ->
            ( state, Nothing )


resetMouseStateWithId : Bool -> String -> State -> State
resetMouseStateWithId separateSelections id state =
    if separateSelections then
        { key = state.key, mouse = Just id }
    else
        { key = Just id, mouse = Just id }


getPreviousItemId : List String -> String -> String
getPreviousItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldr (getPrevious selectedId) Nothing ids


getPrevious : String -> String -> Maybe String -> Maybe String
getPrevious id selectedId resultId =
    if selectedId == id then
        Just id
    else if (Maybe.withDefault "" resultId) == id then
        Just selectedId
    else
        resultId


getNextItemId : List String -> String -> String
getNextItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldl (getPrevious selectedId) Nothing ids


navigateWithKey : Int -> List String -> Maybe String -> Maybe String
navigateWithKey code ids maybeId =
    case code of
        38 ->
            Maybe.map (getPreviousItemId ids) maybeId

        40 ->
            Maybe.map (getNextItemId ids) maybeId

        _ ->
            maybeId


keyedDiv : List (Attribute Msg) -> List (String, Html Msg) -> Html Msg
keyedDiv attributes children =
    (Html.Keyed.node "div") attributes children


view : ViewConfig data -> Int -> State -> List data -> Html Msg
view config howManyToShow state data =
    Html.div [ Html.Attributes.class "dropdown open" ]
        [ viewList config howManyToShow state data ]


viewItem : ViewConfig data -> State -> data -> Html Msg
viewItem { toId, viewItemContent } { key, mouse } data =
    let
        id =
            toId data

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
        Html.button
            [ Html.Attributes.classList
                [ ( "dropdown-item", True )
                , ( "bg-faded", (isSelected key) || (isSelected mouse) )
                ]
            , Html.Attributes.id id
            , Html.Attributes.type' "button"
            , Html.Events.onFocus (Focus id)
            , Html.Events.onMouseEnter (MouseEnter id)
            , Html.Events.onMouseLeave (MouseLeave id)
            , Html.Events.onClick (MouseClick id)
            ]
            (List.map (Html.App.map (\html -> NoOp)) (viewItemContent  data))


viewList : ViewConfig data -> Int -> State -> List data -> Html Msg
viewList config howManyToShow state data =
    let
        getKeyedItems datum =
            ( config.toId datum, viewItem config state datum )
    in
        keyedDiv [ Html.Attributes.class "dropdown-menu" ]
            (List.take howManyToShow data
                |> List.map getKeyedItems
            )


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig data =
    { toId : data -> String
    -- , ul : List (Attribute Never)
    , viewItemContent : data -> List (Html Never)
    }


viewConfig :
    { toId : data -> String
    -- , ul : List (Attribute Never)
    , viewItemContent : data -> List (Html Never)
    }
    -> ViewConfig data
-- viewConfig { toId, ul, viewItemContent } =
viewConfig { toId, viewItemContent } =
    { toId = toId
    -- , ul = ul
    , viewItemContent = viewItemContent
    }
