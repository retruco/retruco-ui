module Statements exposing (..)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Http
import NewStatement
import Task
import Types exposing (Ballot, DataId, DataIdsBody, decodeDataIdsBody, Statement, StatementCustom(..))


-- MODEL


type alias Model =
    { ballotById : Dict String Ballot
    , newStatement : NewStatement.Model
    , statementById : Dict String Statement
    , statementIds : List String
    }


init : Model
init =
    { ballotById = Dict.empty
    , newStatement = NewStatement.init
    , statementById = Dict.empty
    , statementIds = []
    }


-- UPDATE


type Msg
    = Error Http.Error
    | Load
    | Loaded DataIdsBody
    | NewStatementMsg NewStatement.Msg


load : Cmd Msg
load =
    Task.perform (\_ -> Debug.crash "") (\_ -> Load) (Task.succeed "")


update : Msg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg )
update msg authenticationMaybe model =
    case msg of
        Error err ->
            let
                _ = Debug.log "Statemants Error" err
            in
                ( model, Cmd.none )

        Load ->
            let
                cmd =
                    Task.perform
                        Error
                        Loaded
                        (Http.get decodeDataIdsBody "http://localhost:3000/statements")
            in
                ( model, cmd )

        Loaded body ->
            ( { model
                | statementById = body.data.statements
                , statementIds = body.data.ids
              }
            , Cmd.none
            )

        NewStatementMsg subMsg ->
            let
                (newStatement, subEffect, dataMaybe) =
                    NewStatement.update subMsg authenticationMaybe model.newStatement
                model' = case dataMaybe of
                    Just data ->
                        { model
                        | ballotById = Dict.merge
                            (\id ballot ballotById -> if ballot.deleted
                                then ballotById
                                else Dict.insert id ballot ballotById)
                            (\id leftBallot rightBallot ballotById -> if leftBallot.deleted
                                then ballotById
                                else Dict.insert id leftBallot ballotById)
                            Dict.insert
                            data.ballots
                            model.ballotById
                            Dict.empty
                        , newStatement = newStatement
                        , statementById = Dict.merge
                            (\id statement statementById -> if statement.deleted
                                then statementById
                                else Dict.insert id statement statementById)
                            (\id leftStatement rightStatement statementById -> if leftStatement.deleted
                                then statementById
                                else Dict.insert id leftStatement statementById)
                            Dict.insert
                            data.statements
                            model.statementById
                            Dict.empty
                        , statementIds = if Dict.member data.id data.statements
                            then if List.member data.id model.statementIds
                                then model.statementIds
                                else data.id :: model.statementIds
                            else
                                -- data.id is not the ID of a statement (but a ballot ID, etc).
                                model.statementIds
                        }
                    Nothing ->
                        { model
                        | newStatement = newStatement
                        }
            in
                (model', Cmd.map NewStatementMsg subEffect)
            

-- VIEW


view : Maybe Authenticator.Model.Authentication -> Model -> Html Msg
view authenticationMaybe model =
    node "ui-statements"
        []
        [ text "Statements"
        , ul [] (List.map (\id -> li [] [ viewStatementLine id model ]) model.statementIds)
        , case authenticationMaybe of
            Just authentication ->
                Html.App.map NewStatementMsg (NewStatement.view model.newStatement)
            Nothing ->
                text ""
        ]


viewStatementLine : String -> Model -> Html.Html Msg
viewStatementLine id model =
    let
        statementMaybe =
            Dict.get id model.statementById
    in
        case statementMaybe of
            Nothing ->
                div []
                    [ text id
                    , text " "
                    , text "Missing statement"
                    ]

            Just statement ->
                case statement.custom of
                    AbuseCustom abuse ->
                        div []
                            [ text id
                            , text " abuse "
                            , text statement.createdAt
                            ]

                    ArgumentCustom argument ->
                        div []
                            [ text id
                            , text " argument "
                            , text statement.createdAt
                            ]

                    PlainCustom plain ->
                        div []
                            [ text id
                            , text " plain "
                            , text plain.name
                            ]

                    TagCustom tag ->
                        div []
                            [ text id
                            , text " tag "
                            , text tag.name
                            ]
