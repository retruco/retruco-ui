module Statements exposing (..)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (..)
import Html.App
import Http
import NewStatement
import Task
import Types exposing (Ballot, DataId, DataIdsBody, decodeDataIdsBody, Statement, StatementCustom(..))
import Views exposing (aForPath)


-- MODEL


type alias Model =
    { ballotById : Dict String Ballot
    , loaded : Bool
    , newStatement : NewStatement.Model
    , statementById : Dict String Statement
    , statementIds : List String
    }


init : Model
init =
    { ballotById = Dict.empty
    , loaded = False
    , newStatement = NewStatement.init
    , statementById = Dict.empty
    , statementIds = []
    }


-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = Error Http.Error
    | Load
    | Loaded DataIdsBody
    | NewStatementMsg NewStatement.Msg


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg = Msg -> parentMsg


load : Cmd Msg
load =
    Task.perform (\_ -> Debug.crash "") (\_ -> ForSelf Load) (Task.succeed "")


navigate : String -> Msg
navigate path =
    ForParent (Navigate path)


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg {onInternalMsg, onNavigate} msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg


update : InternalMsg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg )
update msg authenticationMaybe model =
    case msg of
        Error err ->
            let
                _ = Debug.log "Statemants Error" err
            in
                ( model, Cmd.none )

        Load ->
            let
                cmd = if model.loaded
                    then
                        Cmd.none
                    else
                        Task.perform
                            (\msg -> ForSelf (Error msg))
                            (\msg -> ForSelf (Loaded msg))
                            (Http.get decodeDataIdsBody "http://localhost:3000/statements")
            in
                ( model, cmd )

        Loaded body ->
            ( { model
                | loaded = True
                , statementById = body.data.statements
                , statementIds = body.data.ids
              }
            , Cmd.none
            )

        NewStatementMsg subMsg ->
            let
                (newStatement, childMsg, dataMaybe) =
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
                (model', Cmd.map (\msg -> ForSelf (NewStatementMsg msg)) childMsg)
            

-- VIEW


view : Maybe Authenticator.Model.Authentication -> Model -> Html Msg
view authenticationMaybe model =
    node "ui-statements"
        []
        [ text "Statements"
        , ul [] (List.map (\id -> li [] [ viewStatementLine id model ]) model.statementIds)
        , case authenticationMaybe of
            Just authentication ->
                Html.App.map (\msg -> ForSelf (NewStatementMsg msg)) (NewStatement.view model.newStatement)
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
                            , aForPath navigate ("/statements/" ++ id) [] [ text plain.name ]
                            ]

                    TagCustom tag ->
                        div []
                            [ text id
                            , text " tag "
                            , text tag.name
                            ]
