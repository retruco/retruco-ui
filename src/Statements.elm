module Statements exposing (..)

import Authenticator.Model
import Dict exposing (Dict)
import Hop.Types
import Html exposing (..)
import Html.App
import Http
import NewStatement
import Statement
import Task
import Routes exposing (StatementsNestedRoute(..))
import Types exposing (Ballot, DataId, DataIdsBody, decodeDataIdsBody, Statement, StatementCustom(..))
import Views exposing (aForPath, viewNotFound, viewStatementLinePanel)


-- MODEL


type alias Model =
    { ballotById : Dict String Ballot
    , loaded : Bool
    -- , location : Hop.Types.Location
    , newStatement : NewStatement.Model
    , route : StatementsNestedRoute
    , statementById : Dict String Statement
    , statementId : String
    , statementIds : List String
    }


init : Model
init =
    { ballotById = Dict.empty
    , loaded = False
    -- , location = Hop.Types.newLocation
    , newStatement = NewStatement.init
    , route = StatementsNotFoundRoute
    , statementById = Dict.empty
    , statementId = "nothing"
    , statementIds = []
    }


-- ROUTING


urlUpdate : (StatementsNestedRoute, Hop.Types.Location) -> Model -> (Model, Cmd Msg)
urlUpdate (route, location) model =
    let
        model' = { model
            -- | location = location
            | route = route
            }
    in
        case route of
            StatementRoute statementId ->
                ({ model' | statementId = statementId }, Cmd.none)

            StatementsIndexRoute ->
                (model', load)

            StatementsNotFoundRoute ->
                (model', Cmd.none)


-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = Error Http.Error
    | Load
    | Loaded DataIdsBody
    | NewStatementMsg NewStatement.Msg
    | StatementMsg Statement.InternalMsg


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


statementMsgTranslation : Statement.MsgTranslation Msg
statementMsgTranslation =
    { onInternalMsg = \internalMsg -> ForSelf (StatementMsg internalMsg)
    , onNavigate = \path -> ForParent (Navigate path)
    }


translateStatementMsg : Statement.MsgTranslator Msg
translateStatementMsg = Statement.translateMsg statementMsgTranslation


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

        NewStatementMsg childMsg ->
            let
                (newStatement, childEffect, dataMaybe) =
                    NewStatement.update childMsg authenticationMaybe model.newStatement
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
                (model', Cmd.map (\msg -> ForSelf (NewStatementMsg msg)) childEffect)

        StatementMsg childMsg ->
            let
                statementModel =
                    { ballotById = model.ballotById
                    , statementById = model.statementById
                    , statementId = model.statementId
                    }
                (statementModel', childEffect) = Statement.update childMsg authenticationMaybe statementModel
                model' =
                    { model
                    | ballotById = statementModel'.ballotById
                    , statementById = statementModel'.statementById
                    }
            in
                (model', Cmd.map translateStatementMsg childEffect)


-- VIEW


view : Maybe Authenticator.Model.Authentication -> Model -> Html Msg
view authenticationMaybe model =
    case model.route of
        StatementRoute statementId ->
            let
                statementModel =
                    { ballotById = model.ballotById
                    , statementById = model.statementById
                    , statementId = model.statementId
                    }
            in
                Html.App.map translateStatementMsg (Statement.view authenticationMaybe statementModel)

        StatementsIndexRoute ->
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

        StatementsNotFoundRoute ->
            viewNotFound


viewStatementLine : String -> Model -> Html Msg
viewStatementLine statementId model =
    let
        statementMaybe =
            Dict.get statementId model.statementById
    in
        case statementMaybe of
            Nothing ->
                div []
                    [ text statementId
                    , text " "
                    , text "Missing statement"
                    ]

            Just statement ->
                case statement.custom of
                    AbuseCustom abuse ->
                        div []
                            [ viewStatementLinePanel statement
                            , text statement.id
                            , text " abuse "
                            , text statement.createdAt
                            ]

                    ArgumentCustom argument ->
                        div []
                            [ viewStatementLinePanel statement
                            , text statement.id
                            , text " argument "
                            , text statement.createdAt
                            ]

                    PlainCustom plain ->
                        div []
                            [ viewStatementLinePanel statement
                            , text statement.id
                            , text " plain "
                            , aForPath navigate ("/statements/" ++ statement.id) [] [ text plain.name ]
                            ]

                    TagCustom tag ->
                        div []
                            [ viewStatementLinePanel statement
                            , text statement.id
                            , text " tag "
                            , text tag.name
                            ]
