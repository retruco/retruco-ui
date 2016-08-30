module Statement exposing (InternalMsg, Msg, MsgTranslation, MsgTranslator, translateMsg, update, view)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (div, Html, li, node, text, ul)
import Types exposing (Ballot, Statement, StatementCustom(..))
import Views exposing (aForPath, viewStatementLinePanel)


-- MODEL


type alias Model =
    { ballotById : Dict String Ballot
    , statementById : Dict String Statement
    , statementId : String
    }


-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = None


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
        None ->
            ( model, Cmd.none )


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
                in
                    node "ui-statement"
                        []
                        [ statementCustomView
                        , ul [] (List.map (\id -> li [] [ viewGroundArgumentLine id model ]) statement.groundIds)
                        -- , case authenticationMaybe of
                        --     Just authentication ->
                        --         Html.App.map (\msg -> ForSelf (NewStatementMsg msg)) (NewStatement.view model.newStatement)
                        --     Nothing ->
                        --         text ""
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
