module NewStatement exposing (init, Msg, Model, update, view)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Encode
import String
import Task
import Types exposing (DataId, DataIdBody, decodeDataIdBody)


-- MODEL


type alias Errors = Dict String String


type alias Model =
    { errors : Errors
    , kind : String
    , languageCode : String
    , name : String
    }


init : Model
init =
    { errors = Dict.empty
    , kind = "PlainStatement"
    , languageCode = "en"
    , name = ""
    }


-- UPDATE


type Msg
    = Created DataIdBody
    | CreateError Http.Error
    | KindChanged String
    | LanguageCodeChanged String
    | NameInput String
    | Rated DataIdBody
    | RateError Http.Error
    | Submit


update : Msg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg, Maybe DataId )
update msg authenticationMaybe model =
    case msg of
        Created body ->
            let
                data = body.data
                ratingBody = Json.Encode.object
                    [ ("rating", Json.Encode.float 1) ]
                cmd =
                    case authenticationMaybe of
                        Just authentication ->
                            Task.perform
                                RateError
                                Rated
                                ( Http.fromJson decodeDataIdBody ( Http.send Http.defaultSettings
                                    { verb = "POST"
                                    , url = ("http://localhost:3000/statements/" ++ data.id
                                        ++ "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=tags")
                                    , headers =
                                        [ ("Accept", "application/json")
                                        , ("Content-Type", "application/json")
                                        , ("Retruco-API-Key", authentication.apiKey)
                                        ]
                                    , body = Http.string ( Json.Encode.encode 2 ratingBody )
                                    } ) )
                        Nothing ->
                            Cmd.none
            in
                (model, cmd, Just data)

        CreateError err ->
            let
                _ = Debug.log "New Statement Create Error" err
            in
                (model, Cmd.none, Nothing)

        KindChanged kind ->
            ({ model | kind = kind }, Cmd.none, Nothing)

        LanguageCodeChanged languageCode ->
            ({ model | languageCode = languageCode }, Cmd.none, Nothing)

        NameInput name ->
            ({ model | name = name }, Cmd.none, Nothing)

        Rated body ->
            (model, Cmd.none, Just body.data)

        RateError err ->
            let
                _ = Debug.log "New Statement Rate Error" err
            in
                (model, Cmd.none, Nothing)

        Submit ->
            let
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
                        , if String.isEmpty model.kind
                            then Just "Missing type"
                            else Nothing
                        )
                    ,
                        ( "languageCpde"
                        , if  model.kind == "PlainStatement" && String.isEmpty model.languageCode
                            then Just "Missing language"
                            else Nothing
                        )
                    ,
                        ( "name"
                        , if List.member model.kind ["PlainStatement", "Tag"] && String.isEmpty model.name
                            then Just "Missing name"
                            else Nothing
                        )
                    ] )

                cmd =
                    if List.isEmpty errorsList then
                        case authenticationMaybe of
                            Just authentication ->
                                let
                                    bodyJson = Json.Encode.object
                                        ( [ ("type", Json.Encode.string model.kind) ]
                                        ++ case model.kind of
                                            "PlainStatement" ->
                                                [ ("languageCode", Json.Encode.string model.languageCode)
                                                , ("name", Json.Encode.string model.name)
                                                ]

                                            "Tag" ->
                                                [ ("name", Json.Encode.string model.name)
                                                ]

                                            _ ->
                                                []
                                        )
                                in
                                    Task.perform
                                        CreateError
                                        Created
                                        ( Http.fromJson decodeDataIdBody ( Http.send Http.defaultSettings
                                            { verb = "POST"
                                            , url = ("http://localhost:3000/statements"
                                                ++ "?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=tags")
                                            , headers =
                                                [ ("Accept", "application/json")
                                                , ("Content-Type", "application/json")
                                                , ("Retruco-API-Key", authentication.apiKey)
                                                ]
                                            , body = Http.string ( Json.Encode.encode 2 bodyJson )
                                            } ) )
                            Nothing ->
                                Cmd.none
                    else
                        Cmd.none
            in
                ({ model | errors = Dict.fromList errorsList }, cmd, Nothing)


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


view : Model -> Html Msg
view model =
    Html.form [ onSubmit Submit ]
        ([ let
                errorMaybe = Dict.get "kind" model.errors
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
                            (viewOption model.kind)
                            kindLabelCouples
                        )
                    ] ++ errorBlock )
        ]
        ++
        ( case model.kind of
            "PlainStatement" ->
                [ let
                        errorMaybe = Dict.get "languageCode" model.errors
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
                                    (viewOption model.languageCode)
                                    languageCodeLabelCouples
                                )
                            ] ++ errorBlock )
                , let
                        errorMaybe = Dict.get "name" model.errors
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
                                , value model.name
                                , onInput NameInput
                                ] ++ errorAttributes )
                                []
                            ] ++ errorBlock )
                ]

            "Tag" ->
                [ let
                        errorMaybe = Dict.get "name" model.errors
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
                                , value model.name
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


viewOption : String -> (String, String) -> Html Msg
viewOption selectedItem (item, label) =
    option
        [ selected (item == selectedItem)
        , value item
        ]
        [ text label ]
