module Statements exposing (..)

import Dict exposing (Dict)
import Html exposing (div, li, node, text, ul)
-- import Html.App
import Http
import Json.Decode as Json exposing ((:=), andThen, dict, fail, list, map, maybe, string, succeed)
import Json.Decode.Extra as Json exposing ((|:))
import Task


-- MODEL


type alias Abuse =
    { createdAt : String
    , id : String
    }


type alias Argument =
    { createdAt : String
    , id : String
    }


type alias Body =
    { data : Data
    }


type alias Data =
    { ids : List String
    , statements : Dict String Statement
    }


type alias Model =
    { byId : Dict String Statement
    , ids : List String
    }


type alias Plain =
    { createdAt : String
    , id : String
    , name : Maybe String
    }


type Statement
    = AbuseStatement Abuse
    | ArgumentStatement Argument
    | PlainStatement Plain
    | TagStatement Tag


type alias Tag =
    { createdAt : String
    , id : String
    }


decodeBody : Json.Decoder Body
decodeBody =
    succeed Body
        |: ("data" := decodeData)


decodeData : Json.Decoder Data
decodeData =
    succeed Data
        |: ("ids" := list string)
        |: ("statements" := dict decodeStatement)


decodeStatement : Json.Decoder Statement
decodeStatement =
    ("type" := string) `andThen` decodeStatementFromType


decodeStatementFromType : String -> Json.Decoder Statement
decodeStatementFromType statementType =
    case statementType of
        "Abuse" ->
            succeed Abuse
                |: ("createdAt" := string)
                |: ("id" := string)
            `andThen` \abuse -> succeed (AbuseStatement abuse)

        "Argument" ->
            succeed Argument
                |: ("createdAt" := string)
                |: ("id" := string)
            `andThen` \argument -> succeed (ArgumentStatement argument)

        "PlainStatement" ->
            succeed Plain
                |: ("createdAt" := string)
                |: ("id" := string)
                |: maybe ("name" := string)
            `andThen` \plain -> succeed (PlainStatement plain)

        "Tag" ->
            succeed Tag
                |: ("createdAt" := string)
                |: ("id" := string)
            `andThen` \tag -> succeed (TagStatement tag)

        _ ->
            fail ("Unkown statement type: " ++ statementType)

init : Model
init =
    { ids = []
    , byId = Dict.empty
    }


-- UPDATE


type Msg
    = Load
    | Loaded Body
    | Error Http.Error


load : Cmd Msg
load =
    Task.perform (\_ -> Debug.crash "") (\_ -> Load) (Task.succeed "")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Error err ->
            let
                errLogged = Debug.log "Error" err
            in
                ( model, Cmd.none )

        Load ->
            let
                cmd =
                    Task.perform
                        Error
                        Loaded
                        (Http.get decodeBody "http://localhost:3000/statements")
            in
                ( model, cmd )

        Loaded body ->
            ( { model
                | byId = body.data.statements
                , ids = body.data.ids
              }
            , Cmd.none
            )


-- VIEW


view : Model -> Html.Html Msg
view model =
    node "ui-statements"
        []
        [ text "Statements"
        , ul [] (List.map (\id -> li [] [ viewStatementLine id model ]) model.ids)
        ]


viewStatementLine : String -> Model -> Html.Html Msg
viewStatementLine id model =
    let
        statementMaybe =
            Dict.get id model.byId
    in
        case statementMaybe of
            Nothing ->
                div []
                    [ text id
                    , text " "
                    , text "Missing statement"
                    ]

            Just statement ->
                case statement of
                    AbuseStatement abuse ->
                        div []
                            [ text id
                            , text " abuse "
                            , text abuse.createdAt
                            ]

                    ArgumentStatement argument ->
                        div []
                            [ text id
                            , text " argument "
                            , text argument.createdAt
                            ]

                    PlainStatement plain ->
                        let
                            nameMaybe = plain.name
                        in
                            case nameMaybe of
                                Nothing ->
                                    div []
                                        [ text id
                                        , text " plain "
                                        ]

                                Just name ->
                                    div []
                                        [ text id
                                        , text " plain "
                                        , text name
                                        ]

                    TagStatement tag ->
                        div []
                            [ text id
                            , text " tag "
                            , text tag.createdAt
                            ]
