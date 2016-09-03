module NewGroundArgument exposing (init, Msg, Model, update, view)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Requests exposing (newTaskCreateStatement, newTaskRateStatement)
import String
import Task
import Types exposing (convertStatementFormToCustom, DataId, DataIdBody, decodeDataIdBody, initStatementForm,
    StatementCustom(..), StatementForm)
import Views exposing (viewOption)


-- MODEL


type alias Model =
    { claimId : String
    , groundId : String
    , rating : Int
    , statementForm : StatementForm
    }


init : Model
init =
    { claimId = ""
    , groundId = ""
    , rating = 0
    , statementForm = initStatementForm
    }


-- UPDATE


type Msg
    = ArgumentCreated DataIdBody
    | ArgumentCreateError Http.Error
    | GroundCreated DataIdBody
    | GroundCreateError Http.Error
    | KindChanged String
    | LanguageCodeChanged String
    | NameInput String
    | ArgumentRated DataIdBody
    | ArgumentRateError Http.Error
    | GroundRated DataIdBody
    | GroundRateError Http.Error
    | RatingChanged Int
    | Submit


update : Msg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg, Maybe DataId )
update msg authenticationMaybe model =
    case msg of
        ArgumentCreated body ->
            let
                data = body.data
                cmd =
                    case authenticationMaybe of
                        Just authentication ->
                            Task.perform
                                ArgumentRateError
                                ArgumentRated
                                (newTaskRateStatement authentication model.rating data.id)

                        Nothing ->
                            Cmd.none
            in
                (model, cmd, Just data)

        ArgumentCreateError err ->
            let
                _ = Debug.log "Argument Create Error" err
            in
                (model, Cmd.none, Nothing)

        ArgumentRated body ->
            (model, Cmd.none, Just body.data)

        ArgumentRateError err ->
            let
                _ = Debug.log "Argumenet Rate Error" err
            in
                (model, Cmd.none, Nothing)

        GroundCreated body ->
            let
                data = body.data
                cmd =
                    case authenticationMaybe of
                        Just authentication ->
                            Task.perform
                                GroundRateError
                                GroundRated
                                (newTaskRateStatement authentication 1 data.id)

                        Nothing ->
                            Cmd.none
            in
                ({ model | groundId = data.id }, cmd, Just data)

        GroundCreateError err ->
            let
                _ = Debug.log "Ground Statement Create Error" err
            in
                (model, Cmd.none, Nothing)

        GroundRated body ->
            let
                cmd =
                    case authenticationMaybe of
                        Just authentication ->
                            Task.perform
                                ArgumentCreateError
                                ArgumentCreated
                                (newTaskCreateStatement authentication (ArgumentCustom
                                    { claimId = model.claimId
                                    , groundId = model.groundId
                                    }))

                        Nothing ->
                            Cmd.none
            in
                (model, cmd, Just body.data)

        GroundRateError err ->
            let
                _ = Debug.log "Ground Statement Rate Error" err
            in
                (model, Cmd.none, Nothing)

        KindChanged kind ->
            let
                statementForm = model.statementForm
                statementForm' =
                    { statementForm
                    | kind = kind
                    }
            in
                ({ model | statementForm = statementForm' }, Cmd.none, Nothing)

        LanguageCodeChanged languageCode ->
            let
                statementForm = model.statementForm
                statementForm' =
                    { statementForm
                    | languageCode = languageCode
                    }
            in
                ({ model | statementForm = statementForm' }, Cmd.none, Nothing)

        NameInput name ->
            let
                statementForm = model.statementForm
                statementForm' =
                    { statementForm
                    | name = name
                    }
            in
                ({ model | statementForm = statementForm' }, Cmd.none, Nothing)

        RatingChanged rating ->
            ({ model | rating = rating }, Cmd.none, Nothing)

        Submit ->
            let
                statementForm = model.statementForm
                errorsList = ( List.filterMap (
                    \(name, errorMaybe) ->
                        case errorMaybe of
                            Just error ->
                                Just (name, error)
                            Nothing ->
                                Nothing
                    )
                    [
                        ( "kind"
                        , if String.isEmpty statementForm.kind
                            then Just "Missing type"
                            else Nothing
                        )
                    ,
                        ( "languageCpde"
                        , if  statementForm.kind == "PlainStatement" && String.isEmpty statementForm.languageCode
                            then Just "Missing language"
                            else Nothing
                        )
                    ,
                        ( "name"
                        , if List.member statementForm.kind ["PlainStatement", "Tag"]
                            && String.isEmpty statementForm.name
                            then Just "Missing name"
                            else Nothing
                        )
                    ] )

                cmd =
                    if List.isEmpty errorsList then
                        case authenticationMaybe of
                            Just authentication ->
                                Task.perform
                                    GroundCreateError
                                    GroundCreated
                                    (newTaskCreateStatement
                                        authentication
                                        (convertStatementFormToCustom statementForm))
                            Nothing ->
                                Cmd.none
                    else
                        Cmd.none
                statementForm' =
                    { statementForm
                    | errors = Dict.fromList errorsList
                    }
            in
                ({ model | statementForm = statementForm' }, cmd, Nothing)


-- VIEW


kindLabelCouples : List (String, String)
kindLabelCouples =
    [ ("PlainStatement", "Plain")
    , ("Tag", "Tag")
    ]


kinds : List String
kinds = List.map (\(item, label) -> item) kindLabelCouples


kindTargetValueDecoder : Json.Decode.Decoder String
kindTargetValueDecoder =
    targetValue `Json.Decode.andThen` \value ->
        if List.member value kinds then
            Json.Decode.succeed value
        else
            Json.Decode.fail ("Unknown type: " ++ value)


languageCodeLabelCouples : List (String, String)
languageCodeLabelCouples =
    [ ("en", "English")
    , ("fr", "Français")
    ]


languageCodes : List String
languageCodes = List.map (\(item, label) -> item) languageCodeLabelCouples


languageCodeTargetValueDecoder : Json.Decode.Decoder String
languageCodeTargetValueDecoder =
    targetValue `Json.Decode.andThen` \value ->
        if List.member value languageCodes then
            Json.Decode.succeed value
        else
            Json.Decode.fail ("Unknown language: " ++ value)


ratingLabelCouples : List (Int, String)
ratingLabelCouples =
    [ (1, "Because")
    , (0, "However")
    , (-1, "But")
    ]


ratings : List Int
ratings = List.map (\(item, label) -> item) ratingLabelCouples


ratingTargetValueDecoder : Json.Decode.Decoder Int
ratingTargetValueDecoder =
    Json.Decode.customDecoder targetValue (Json.Decode.decodeString Json.Decode.int) `Json.Decode.andThen` \value ->
        if List.member value ratings then
            Json.Decode.succeed value
        else
            Json.Decode.fail ("Unknown rating: " ++ toString value)


view : Model -> Html Msg
view model =
    let
        statementForm = model.statementForm
    in
    Html.form [ onSubmit Submit ]
        ([ let
                errorMaybe = Dict.get "rating" statementForm.errors
                ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
                    Just error ->
                        ( " has-error"
                        , [ ariaDescribedby "rating-error" ]
                        , [ span 
                            [ class "help-block"
                            , id "rating-error"
                            ]
                            [ text error ] ]
                        )
                    Nothing ->
                        ("", [] , [])
            in
                div [ class ( "form-group" ++ errorClass) ]
                    ( [ label [ class "control-label", for "rating" ] [ text "Rating" ]
                    , select
                        ( [ class "form-control"
                        , id "rating"
                        , on "change" (Json.Decode.map RatingChanged ratingTargetValueDecoder)
                        ] ++ errorAttributes )
                        ( List.map
                            (viewOption model.rating)
                            ratingLabelCouples
                        )
                    ] ++ errorBlock )
        , let
                errorMaybe = Dict.get "kind" statementForm.errors
                ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
                    Just error ->
                        ( " has-error"
                        , [ ariaDescribedby "type-error" ]
                        , [ span 
                            [ class "help-block"
                            , id "type-error"
                            ]
                            [ text error ] ]
                        )
                    Nothing ->
                        ("", [] , [])
            in
                div [ class ( "form-group" ++ errorClass) ]
                    ( [ label [ class "control-label", for "type" ] [ text "Type" ]
                    , select
                        ( [ class "form-control"
                        , id "type"
                        , on "change" (Json.Decode.map KindChanged kindTargetValueDecoder)
                        ] ++ errorAttributes )
                        ( List.map
                            (viewOption statementForm.kind)
                            kindLabelCouples
                        )
                    ] ++ errorBlock )
        ]
        ++
        ( case statementForm.kind of
            "PlainStatement" ->
                [ let
                        errorMaybe = Dict.get "languageCode" statementForm.errors
                        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
                            Just error ->
                                ( " has-error"
                                , [ ariaDescribedby "language-error" ]
                                , [ span 
                                    [ class "help-block"
                                    , id "language-error"
                                    ]
                                    [ text error ] ]
                                )
                            Nothing ->
                                ("", [] , [])
                    in
                        div [ class ( "form-group" ++ errorClass) ]
                            ( [ label [ class "control-label", for "language" ] [ text "Language" ]
                            , select
                                ( [ class "form-control"
                                , id "language"
                                , on "change" (Json.Decode.map LanguageCodeChanged languageCodeTargetValueDecoder)
                                ] ++ errorAttributes )
                                ( List.map
                                    (viewOption statementForm.languageCode)
                                    languageCodeLabelCouples
                                )
                            ] ++ errorBlock )
                , let
                        errorMaybe = Dict.get "name" statementForm.errors
                        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
                            Just error ->
                                ( " has-error"
                                , [ ariaDescribedby "name-error" ]
                                , [ span 
                                    [ class "help-block"
                                    , id "name-error"
                                    ]
                                    [ text error ] ]
                                )
                            Nothing ->
                                ("", [] , [])
                    in
                        div [ class ( "form-group" ++ errorClass) ]
                            ( [ label [ class "control-label", for "name" ] [ text "Name" ]
                            , input
                                ( [ class "form-control"
                                , id "name"
                                , placeholder "To be or not to be"
                                , type' "text"
                                , value statementForm.name
                                , onInput NameInput
                                ] ++ errorAttributes )
                                []
                            ] ++ errorBlock )
                ]

            "Tag" ->
                [ let
                        errorMaybe = Dict.get "name" statementForm.errors
                        ( errorClass, errorAttributes, errorBlock ) = case errorMaybe of
                            Just error ->
                                ( " has-error"
                                , [ ariaDescribedby "name-error" ]
                                , [ span 
                                    [ class "help-block"
                                    , id "name-error"
                                    ]
                                    [ text error ] ]
                                )
                            Nothing ->
                                ("", [] , [])
                    in
                        div [ class ( "form-group" ++ errorClass) ]
                            ( [ label [ class "control-label", for "name" ] [ text "Name" ]
                            , input
                                ( [ class "form-control"
                                , id "name"
                                , placeholder "To be or not to be"
                                , type' "text"
                                , value statementForm.name
                                , onInput NameInput
                                ] ++ errorAttributes )
                                []
                            ] ++ errorBlock )
                ]

            _ ->
                []
        ) ++ 
        [ button
            [ class "btn btn-primary", type' "submit" ]
            [ text "Create" ]
        ])
