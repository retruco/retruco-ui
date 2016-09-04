module Requests exposing (newTaskCreateStatement, newTaskGetStatements, newTaskRateStatement)

import Authenticator.Model
import Http
import Json.Encode
import Types exposing (convertStatementCustomToKind, DataIdBody, DataIdsBody, decodeDataIdBody, decodeDataIdsBody,
    StatementCustom(..))
import Task exposing (Task)


newTaskCreateStatement : Authenticator.Model.Authentication -> StatementCustom -> Task Http.Error DataIdBody
newTaskCreateStatement authentication statementCustom =
    let
        bodyJson = Json.Encode.object
            ( [ ("type", Json.Encode.string (convertStatementCustomToKind statementCustom)) ]
            ++ case statementCustom of
                AbuseCustom abuse ->
                    [ ("statementId", Json.Encode.string abuse.statementId)
                    ]

                ArgumentCustom argument ->
                    [ ("claimId", Json.Encode.string argument.claimId)
                    , ("groundId", Json.Encode.string argument.groundId)
                    ]

                PlainCustom plain ->
                    [ ("languageCode", Json.Encode.string plain.languageCode)
                    , ("name", Json.Encode.string plain.name)
                    ]

                TagCustom tag ->
                    [ ("name", Json.Encode.string tag.name)
                    ]
            )
    in
        Http.fromJson decodeDataIdBody ( Http.send Http.defaultSettings
            { verb = "POST"
            , url = ("http://localhost:3000/statements"
                ++ "?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=tags")
            , headers =
                [ ("Accept", "application/json")
                , ("Content-Type", "application/json")
                , ("Retruco-API-Key", authentication.apiKey)
                ]
            , body = Http.string ( Json.Encode.encode 2 bodyJson )
            } )


newTaskGetStatements : Maybe Authenticator.Model.Authentication -> Task Http.Error DataIdsBody
newTaskGetStatements authenticationMaybe =
    let
        authenticationHeaders = case authenticationMaybe of
            Just authentication ->
                [ ("Retruco-API-Key", authentication.apiKey)
                ]
            Nothing ->
                []
    in
        Http.fromJson decodeDataIdsBody ( Http.send Http.defaultSettings
            { verb = "GET"
            , url = ("http://localhost:3000/statements"
                ++ "?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=tags")
            , headers =
                [ ("Accept", "application/json")
                ] ++ authenticationHeaders
            , body = Http.empty
            } )


newTaskRateStatement : Authenticator.Model.Authentication -> Int -> String -> Task Http.Error DataIdBody
newTaskRateStatement authentication rating statementId =
    let
        bodyJson = Json.Encode.object
            [ ("rating", Json.Encode.int rating) ]
    in
        Http.fromJson decodeDataIdBody ( Http.send Http.defaultSettings
            { verb = "POST"
            , url = ("http://localhost:3000/statements/" ++ statementId
                ++ "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=tags")
            , headers =
                [ ("Accept", "application/json")
                , ("Content-Type", "application/json")
                , ("Retruco-API-Key", authentication.apiKey)
                ]
            , body = Http.string ( Json.Encode.encode 2 bodyJson )
            } )
