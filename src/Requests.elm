module Requests
    exposing
        ( newTaskAutocompleteStatements
        , newTaskCreateStatement
        , newTaskDeleteStatementRating
        , newTaskFlagAbuse
        , newTaskGetStatements
        , newTaskRateStatement
        , updateFromDataId
        )

import Authenticator.Model
import Configuration exposing (apiUrl)
import Dict exposing (Dict)
import Http
import Json.Encode
import Task exposing (Task)
import Types
    exposing
        ( Ballot
        , convertArgumentTypeToString
        , convertStatementCustomToKind
        , DataId
        , DataIdBody
        , DataIdsBody
        , decodeDataIdBody
        , decodeDataIdsBody
        , decodeStatementsAutocompletionBody
        , ModelFragment
        , SearchCriteria
        , Statement
        , StatementCustom(..)
        , StatementsAutocompletionBody
        )


newTaskAutocompleteStatements :
    Maybe Authenticator.Model.Authentication
    -> String
    -> String
    -> Int
    -> Task Http.Error StatementsAutocompletionBody
newTaskAutocompleteStatements authenticationMaybe statementType term limit =
    let
        authenticationHeaders =
            case authenticationMaybe of
                Just authentication ->
                    [ ( "Retruco-API-Key", authentication.apiKey )
                    ]

                Nothing ->
                    []
    in
        Http.fromJson decodeStatementsAutocompletionBody
            (Http.send Http.defaultSettings
                { verb = "GET"
                , url =
                    Http.url (apiUrl ++ "statements/autocomplete")
                        [ ( "limit", toString limit )
                        , ( "term", term )
                        , ( "type", statementType )
                        ]
                , headers = ( "Accept", "application/json" ) :: authenticationHeaders
                , body = Http.empty
                }
            )


newTaskCreateStatement : Authenticator.Model.Authentication -> StatementCustom -> Task Http.Error DataIdBody
newTaskCreateStatement authentication statementCustom =
    let
        bodyJson =
            Json.Encode.object
                ([ ( "type", Json.Encode.string (convertStatementCustomToKind statementCustom) ) ]
                    ++ case statementCustom of
                        AbuseCustom abuse ->
                            [ ( "statementId", Json.Encode.string abuse.statementId )
                            ]

                        ArgumentCustom argument ->
                            [ ( "argumentType", Json.Encode.string (convertArgumentTypeToString argument.argumentType) )
                            , ( "claimId", Json.Encode.string argument.claimId )
                            , ( "groundId", Json.Encode.string argument.groundId )
                            ]

                        CitationCustom citation ->
                            [ ( "citedId", Json.Encode.string citation.citedId )
                            , ( "eventId", Json.Encode.string citation.eventId )
                            , ( "personId", Json.Encode.string citation.personId )
                            ]

                        EventCustom event ->
                            [ ( "name", Json.Encode.string event.name )
                            ]

                        PersonCustom person ->
                            [ ( "name", Json.Encode.string person.name )
                            , ( "twitterName", Json.Encode.string person.twitterName )
                            ]

                        PlainCustom plain ->
                            [ ( "languageCode", Json.Encode.string plain.languageCode )
                            , ( "name", Json.Encode.string plain.name )
                            ]

                        TagCustom tag ->
                            [ ( "name", Json.Encode.string tag.name )
                            ]
                )
    in
        Http.fromJson decodeDataIdBody
            (Http.send Http.defaultSettings
                { verb = "POST"
                , url =
                    (apiUrl
                        ++ "statements"
                        ++ "?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
                        ++ "&show=tags"
                    )
                , headers =
                    [ ( "Accept", "application/json" )
                    , ( "Content-Type", "application/json" )
                    , ( "Retruco-API-Key", authentication.apiKey )
                    ]
                , body = Http.string (Json.Encode.encode 2 bodyJson)
                }
            )


newTaskDeleteStatementRating : Authenticator.Model.Authentication -> String -> Task Http.Error DataIdBody
newTaskDeleteStatementRating authentication statementId =
    Http.fromJson decodeDataIdBody
        (Http.send Http.defaultSettings
            { verb = "DELETE"
            , url =
                (apiUrl
                    ++ "statements/"
                    ++ statementId
                    ++ "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
                    ++ "&show=tags"
                )
            , headers =
                [ ( "Accept", "application/json" )
                , ( "Retruco-API-Key", authentication.apiKey )
                ]
            , body = Http.empty
            }
        )


newTaskFlagAbuse : Authenticator.Model.Authentication -> String -> Task Http.Error DataIdBody
newTaskFlagAbuse authentication statementId =
    Http.fromJson decodeDataIdBody
        (Http.send Http.defaultSettings
            { verb = "GET"
            , url =
                (apiUrl
                    ++ "statements/"
                    ++ statementId
                    ++ "/abuse?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
                    ++ "&show=tags"
                )
            , headers =
                [ ( "Accept", "application/json" )
                , ( "Retruco-API-Key", authentication.apiKey )
                ]
            , body = Http.empty
            }
        )


newTaskGetStatements : Maybe Authenticator.Model.Authentication -> SearchCriteria -> Task Http.Error DataIdsBody
newTaskGetStatements authenticationMaybe searchCriteria =
    let
        authenticationHeaders =
            case authenticationMaybe of
                Just authentication ->
                    [ ( "Retruco-API-Key", authentication.apiKey )
                    ]

                Nothing ->
                    []
    in
        Http.fromJson decodeDataIdsBody
            (Http.send Http.defaultSettings
                { verb = "GET"
                , url =
                    Http.url (apiUrl ++ "statements")
                        ([ ( "depth", "1" )
                         , ( "show", "abuse" )
                         , ( "show", "author" )
                         , ( "show", "ballot" )
                         , ( "show", "grounds" )
                         , ( "show", "properties" )
                         , ( "show", "tags" )
                         ]
                            ++ (case searchCriteria.languageCodeMaybe of
                                    Just languageCode ->
                                        [ ( "languageCode", languageCode ) ]

                                    Nothing ->
                                        []
                               )
                            ++ (case searchCriteria.termMaybe of
                                    Just term ->
                                        [ ( "term", term ) ]

                                    Nothing ->
                                        []
                               )
                            ++ List.map (\kind -> ( "type", kind )) searchCriteria.kinds
                        )
                , headers =
                    [ ( "Accept", "application/json" )
                    ]
                        ++ authenticationHeaders
                , body = Http.empty
                }
            )


newTaskRateStatement : Authenticator.Model.Authentication -> Int -> String -> Task Http.Error DataIdBody
newTaskRateStatement authentication rating statementId =
    let
        bodyJson =
            Json.Encode.object
                [ ( "rating", Json.Encode.int rating ) ]
    in
        Http.fromJson decodeDataIdBody
            (Http.send Http.defaultSettings
                { verb = "POST"
                , url =
                    (apiUrl
                        ++ "statements/"
                        ++ statementId
                        ++ "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties"
                        ++ "&show=references&show=tags"
                    )
                , headers =
                    [ ( "Accept", "application/json" )
                    , ( "Content-Type", "application/json" )
                    , ( "Retruco-API-Key", authentication.apiKey )
                    ]
                , body = Http.string (Json.Encode.encode 2 bodyJson)
                }
            )


updateFromDataId : DataId -> ModelFragment a -> ModelFragment a
updateFromDataId data model =
    { model
        | ballotById =
            Dict.merge
                (\id ballot ballotById ->
                    if ballot.deleted then
                        ballotById
                    else
                        Dict.insert id ballot ballotById
                )
                (\id leftBallot rightBallot ballotById ->
                    if leftBallot.deleted then
                        ballotById
                    else
                        Dict.insert id leftBallot ballotById
                )
                Dict.insert
                data.ballots
                model.ballotById
                Dict.empty
        , statementById =
            Dict.merge
                (\id statement statementById ->
                    if statement.deleted then
                        statementById
                    else
                        Dict.insert id statement statementById
                )
                (\id leftStatement rightStatement statementById ->
                    if leftStatement.deleted then
                        statementById
                    else
                        Dict.insert id leftStatement statementById
                )
                Dict.insert
                data.statements
                model.statementById
                Dict.empty
        , statementIds =
            if Dict.member data.id data.statements then
                if List.member data.id model.statementIds then
                    model.statementIds
                else
                    data.id :: model.statementIds
            else
                -- data.id is not the ID of a statement (but a ballot ID, etc).
                model.statementIds
    }
