module Statement exposing (init, InternalMsg, Model, Msg, MsgTranslation, MsgTranslator, translateMsg, update, view)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (div, Html, li, node, text, ul)
import Html.App
import NewGroundArgument
import Types exposing (Ballot, Statement, StatementCustom(..))
import Views exposing (aForPath, viewGroundArgumentLinePanel)


-- MODEL


type alias Model =
    { ballotById : Dict String Ballot
    , newGroundArgumentModel : NewGroundArgument.Model
    , statementById : Dict String Statement
    , statementId : String
    , statementIds : List String
    }


init : Model
init =
    { ballotById = Dict.empty
    , newGroundArgumentModel = NewGroundArgument.init
    , statementById = Dict.empty
    , statementId = ""
    , statementIds = []
    }


-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = NewGroundArgumentMsg NewGroundArgument.Msg


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg = Msg -> parentMsg


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
        NewGroundArgumentMsg childMsg ->
            let
                newGroundArgumentModel = model.newGroundArgumentModel
                newGroundArgumentModel' =
                    { newGroundArgumentModel
                    | claimId = model.statementId
                    }
                (newGroundArgumentModel'', childEffect, dataMaybe) =
                    NewGroundArgument.update childMsg authenticationMaybe newGroundArgumentModel'
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
                        , newGroundArgumentModel = newGroundArgumentModel''
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
                        | newGroundArgumentModel = newGroundArgumentModel''
                        }
            in
                (model', Cmd.map (\msg -> ForSelf (NewGroundArgumentMsg msg)) childEffect)


-- VIEW


view : Maybe Authenticator.Model.Authentication -> Model -> Html Msg
view authenticationMaybe model =
    let
        statementMaybe = Dict.get model.statementId model.statementById
    in
        case statementMaybe of
            Nothing ->
                div []
                    [ text model.statementId
                    , text " "
                    , text "Missing statement"
                    ]

            Just statement ->
                let
                    statementCustomView = case statement.custom of
                        AbuseCustom abuse ->
                            div []
                                [ viewGroundArgumentLinePanel statement
                                , text statement.id
                                , text " abuse "
                                , text statement.createdAt
                                ]

                        ArgumentCustom argument ->
                            div []
                                [ viewGroundArgumentLinePanel statement
                                , text statement.id
                                , text " argument "
                                , text statement.createdAt
                                ]

                        PlainCustom plain ->
                            div []
                                [ viewGroundArgumentLinePanel statement
                                , text statement.id
                                , text " plain "
                                , aForPath navigate ("/statements/" ++ statement.id) [] [ text plain.name ]
                                ]

                        TagCustom tag ->
                            div []
                                [ viewGroundArgumentLinePanel statement
                                , text statement.id
                                , text " tag "
                                , text tag.name
                                ]
                in
                    node "ui-statement"
                        []
                        [ statementCustomView
                        , ul [] (List.map (\id -> li [] [ viewGroundArgumentLine id model ]) statement.groundIds)
                        , case authenticationMaybe of
                            Just authentication ->
                                Html.App.map
                                    (\msg -> ForSelf (NewGroundArgumentMsg msg))
                                    (NewGroundArgument.view model.newGroundArgumentModel)
                            Nothing ->
                                text ""
                        ]


viewGroundArgumentLine : String -> Model -> Html Msg
viewGroundArgumentLine statementId model =
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
                            [ viewGroundArgumentLinePanel statement
                            , text statement.id
                            , text " abuse "
                            , text statement.createdAt
                            ]

                    ArgumentCustom argument ->
                        div []
                            [ viewGroundArgumentLinePanel statement
                            , text statement.id
                            , text " argument "
                            , text statement.createdAt
                            ]

                    PlainCustom plain ->
                        div []
                            [ viewGroundArgumentLinePanel statement
                            , text statement.id
                            , text " plain "
                            , aForPath navigate ("/statements/" ++ statement.id) [] [ text plain.name ]
                            ]

                    TagCustom tag ->
                        div []
                            [ viewGroundArgumentLinePanel statement
                            , text statement.id
                            , text " tag "
                            , text tag.name
                            ]
