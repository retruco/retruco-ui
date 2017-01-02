module Requests exposing (..)

import Authenticator.Types exposing (Authentication)
import Configuration exposing (apiUrl)
import Decoders exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Json.Decode as Decode
import Json.Encode as Encode
import Regex
import String
import Types exposing (..)
import Urls


idRegex : Regex.Regex
idRegex =
    Regex.regex "(^|/)(\\d+)(\\?|$)"


activateUser : String -> String -> Http.Request UserBody
activateUser userId authorization =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-Control" "no-cache" ]
        , url = apiUrl ++ "users/" ++ userId ++ "/activate?authorization=" ++ authorization
        , body = Http.emptyBody
        , expect = Http.expectJson userBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


authenticationHeaders : Maybe Authentication -> List Http.Header
authenticationHeaders authentication =
    case authentication of
        Just authentication ->
            [ Http.header "Retruco-API-Key" authentication.apiKey
            , Http.header "Cache-Control" "no-cache"
              -- Don't cache API requests when user is logged.
            ]

        Nothing ->
            []


autocompleteCards :
    Maybe Authentication
    -> I18n.Language
    -> Maybe String
    -> String
    -> Int
    -> Http.Request CardsAutocompletionBody
autocompleteCards authentication language subType term limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "cards/autocomplete"
                ++ Urls.paramsToQuery
                    [ ( "language", Just (I18n.iso639_1FromLanguage language) )
                    , ( "limit", Just (toString limit) )
                    , ( "term"
                      , let
                            cleanTerm =
                                String.trim term
                        in
                            if String.isEmpty cleanTerm then
                                Nothing
                            else
                                Just cleanTerm
                      )
                    , ( "type", subType )
                    ]
        , body = Http.emptyBody
        , expect = Http.expectJson cardsAutocompletionBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- autocompleteObjects :
--     Maybe Authentication
--     -> String
--     -> String
--     -> Int
--     -> Http.Request StatementsAutocompletionBody
-- autocompleteObjects authentication statementType term limit =
--     Http.request
--         { method = "GET"
--         , headers = authenticationHeaders authentication
--         , url =
--             Http.url (apiUrl ++ "statements/autocomplete")
--                 [ ( "limit", toString limit )
--                 , ( "term", term )
--                 , ( "type", statementType )
--                 ]
--         , body = Http.emptyBody
--         , expect = Http.expectJson decodeStatementsAutocompletionBody
--         , timeout = Nothing
--         , withCredentials = False
--         }


extractId : String -> Maybe String
extractId url =
    (Regex.find Regex.All idRegex url
        |> List.head
    )
        |> Maybe.andThen
            (\match ->
                case match.submatches |> List.drop 1 |> List.head of
                    Nothing ->
                        Nothing

                    Just maybe ->
                        maybe
            )


getCard : Maybe Authentication -> String -> Http.Request DataIdBody
getCard authentication cardId =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "objects/" ++ cardId ++ "?show=references&show=values&depth=2"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCards : Maybe Authentication -> String -> Maybe Int -> List String -> List String -> Http.Request DataIdsBody
getCards authentication term limit tagIds cardTypes =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "cards?"
                ++ (List.map (\cardType -> "type=" ++ cardType) cardTypes
                        ++ (([ Just "show=values"
                             , Just "depth=1"
                             , (if String.isEmpty term then
                                    Nothing
                                else
                                    Just ("term=" ++ term)
                               )
                             , limit |> Maybe.map (\limit -> "limit=" ++ (toString limit))
                             ]
                                |> List.filterMap identity
                            )
                                ++ (tagIds
                                        |> List.filter (\s -> not (String.isEmpty s))
                                        |> List.map (\tagId -> "tag=" ++ tagId)
                                   )
                           )
                        |> String.join "&"
                   )
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCollection : Maybe Authentication -> String -> Http.Request DataIdBody
getCollection authentication collectionId =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "collections/" ++ collectionId ++ "?show=values&depth=3"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCollections : Maybe Authentication -> Maybe Int -> Http.Request DataIdsBody
getCollections authentication limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "collections?show=values&depth=1"
                ++ (case limit of
                        Nothing ->
                            ""

                        Just limit ->
                            "&limit=" ++ (toString limit)
                   )
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCollectionsForAuthor : Authentication -> Http.Request DataIdsBody
getCollectionsForAuthor authentication =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders (Just authentication)
        , url = apiUrl ++ "users/" ++ authentication.urlName ++ "/collections?show=values&depth=1"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getObjectProperties : Maybe Authentication -> String -> String -> Http.Request DataIdsBody
getObjectProperties authentication objectId keyId =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "objects/" ++ objectId ++ "/properties/" ++ keyId ++ "?show=ballots&show=values&depth=1"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getTagsPopularity : Maybe Authentication -> List String -> Http.Request PopularTagsData
getTagsPopularity authentication tagIds =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "cards/tags-popularity?type=use-case&"
                ++ (tagIds
                        |> List.filter (\s -> not (String.isEmpty s))
                        |> List.map (\tagId -> "tag=" ++ tagId)
                        |> String.join "&"
                   )
        , body = Http.emptyBody
        , expect = Http.expectJson popularTagsDataDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getValue : Maybe Authentication -> String -> Http.Request DataIdBody
getValue authentication id =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "objects/" ++ id ++ "?depth=3&show=properties&show=values"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getValues : Maybe Authentication -> Maybe String -> Maybe Int -> Http.Request DataIdsBody
getValues authentication term limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "values?"
                ++ ([ Just "depth=3"
                    , Just "show=properties"
                    , Just "show=values"
                    , (case term of
                        Just "" ->
                            Nothing

                        Just term ->
                            Just ("term=" ++ Http.encodeUri term)

                        Nothing ->
                            Nothing
                      )
                    , limit |> Maybe.map (\limit -> "limit=" ++ (toString limit))
                    ]
                        |> List.filterMap identity
                        |> String.join "&"
                   )
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


postCard : Maybe Authentication -> Dict String String -> I18n.Language -> Http.Request DataIdBody
postCard authentication fields language =
    let
        languageIso639_1 =
            I18n.iso639_1FromLanguage language

        localizedStringEncoder x =
            Encode.object [ ( languageIso639_1, Encode.string x ) ]

        body =
            Encode.object
                -- Always use en(glish) language because this is the language of the labels below.
                -- [ ( "language", Encode.string languageIso639_1 )
                [ ( "language", Encode.string "en" )
                , ( "schemas"
                  , Encode.object
                        [ ( "Description", Encode.string "schema:localized-string" )
                        , ( "Download", Encode.string "schema:uri" )
                        , ( "Logo", Encode.string "schema:uri" )
                        , ( "Name", Encode.string "schema:localized-string" )
                        , ( "Types", Encode.string "schema:value-id" )
                        , ( "Website", Encode.string "schema:uri" )
                        ]
                  )
                , ( "values"
                  , [ ( "Description", localizedStringEncoder )
                    , ( "Download", Encode.string )
                    , ( "Logo", Encode.string )
                    , ( "Name", localizedStringEncoder )
                    , ( "Types", Encode.string )
                    , ( "Website", Encode.string )
                    ]
                        |> List.filterMap
                            (\( k, encoder ) ->
                                Maybe.map (\v -> ( k, encoder v )) (Dict.get k fields)
                            )
                        |> Encode.object
                  )
                , ( "widgets", Encode.object [] )
                ]
                |> Http.jsonBody
    in
        Http.request
            { method = "POST"
            , headers = authenticationHeaders authentication
            , url = apiUrl ++ "cards/easy"
            , body = body
            , expect = Http.expectJson dataIdBodyDecoder
            , timeout = Nothing
            , withCredentials = False
            }



-- postCollection :
--     Maybe Authentication
--     -> Maybe String
--     -> AddNewCollectionFields
--     -> String
--     -> Http.Request DataIdBody
-- postCollection authentication editedCollectionId fields imagePath =
--     let
--         cardIds : List String
--         cardIds =
--             String.words fields.cardIds |> List.filterMap extractId
--         url =
--             case editedCollectionId of
--                 Just collectionId ->
--                     apiUrl ++ "collections/" ++ collectionId
--                 Nothing ->
--                     apiUrl ++ "collections"
--     in
--         Http.request
--             { method = "POST"
--             , headers = authenticationHeaders authentication
--             , url = url
--             , body =
--                 Encode.object
--                     [ ( "cardIds", Encode.list (List.map Encode.string cardIds) )
--                     , ( "description", Encode.string fields.description )
--                     , ( "logo", Encode.string imagePath )
--                     , ( "name", Encode.string fields.name )
--                     ]
--                     |> Http.jsonBody
--             , expect = Http.expectJson dataIdBodyDecoder
--             , timeout = Nothing
--             , withCredentials = False
--             }


postProperty : Maybe Authentication -> String -> String -> String -> Http.Request DataIdBody
postProperty authentication objectId keyId valueId =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "properties?show=ballots&show=values&depth=3"
        , body =
            Encode.object
                [ ( "keyId", Encode.string keyId )
                , ( "objectId", Encode.string objectId )
                , ( "valueId", Encode.string valueId )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


postUploadImage : Maybe Authentication -> String -> Http.Request String
postUploadImage authentication contents =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "uploads/images/json"
        , body =
            Encode.object [ ( "file", Encode.string contents ) ]
                |> Http.jsonBody
        , expect = Http.expectJson (Decode.at [ "data", "path" ] Decode.string)
        , timeout = Nothing
        , withCredentials = False
        }


postValue : Maybe Authentication -> Field -> Http.Request DataIdBody
postValue authentication field =
    let
        ( schemaId, widgetId, encodedValue ) =
            case field of
                BooleanField bool ->
                    ( "schema:boolean", "widget:input-checkbox", Encode.bool bool )

                CardIdField string ->
                    case extractId string of
                        Just cardId ->
                            ( "schema:card-id", "widget:autocomplete", Encode.string cardId )

                        Nothing ->
                            -- TODO: Improve errors handling.
                            ( "schema:string", "widget:input-text", Encode.string string )

                ImageField string ->
                    ( "schema:uri", "widget:image", Encode.string string )

                InputEmailField string ->
                    ( "schema:email", "widget:input-email", Encode.string string )

                InputNumberField float ->
                    ( "schema:number", "widget:input-number", Encode.float float )

                InputTextField string ->
                    ( "schema:string", "widget:input-text", Encode.string string )

                InputUrlField string ->
                    ( "schema:uri", "widget:input-url", Encode.string string )

                LocalizedInputTextField language string ->
                    ( "schema:localized-string"
                    , "widget:input-text"
                    , Encode.object
                        [ ( language, Encode.string string )
                        ]
                    )

                LocalizedTextareaField language string ->
                    ( "schema:localized-string"
                    , "widget:textarea"
                    , Encode.object
                        [ ( language, Encode.string string )
                        ]
                    )

                TextareaField string ->
                    ( "schema:string", "widget:textarea", Encode.string string )
    in
        Http.request
            { method = "POST"
            , headers = authenticationHeaders authentication
            , url = apiUrl ++ "values"
            , body =
                Encode.object
                    [ ( "schema", Encode.string schemaId )
                    , ( "value", encodedValue )
                    , ( "widget", Encode.string widgetId )
                    ]
                    |> Http.jsonBody
            , expect = Http.expectJson dataIdBodyDecoder
            , timeout = Nothing
            , withCredentials = False
            }


rateProperty : Maybe Authentication -> String -> Int -> Http.Request DataIdBody
rateProperty authentication propertyId rating =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "statements/" ++ propertyId ++ "/rating?show=ballots&depth=1"
        , body = Encode.object [ ( "rating", Encode.int rating ) ] |> Http.jsonBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


rateStatement : Authentication -> Int -> String -> Http.Request DataIdBody
rateStatement authentication rating statementId =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders (Just authentication)
        , url =
            (apiUrl
                ++ "statements/"
                ++ statementId
                -- TODO: query parameters
                ++
                    "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties"
                ++ "&show=references&show=tags"
            )
        , body = Encode.object [ ( "rating", Encode.int rating ) ] |> Http.jsonBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


resetPassword : String -> String -> String -> Http.Request UserBody
resetPassword userId authorization password =
    let
        bodyJson =
            Encode.object
                [ ( "password", Encode.string password )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Cache-Control" "no-cache" ]
            , url = apiUrl ++ "users/" ++ userId ++ "/reset-password?authorization=" ++ authorization
            , body = (Http.stringBody "application/json" <| Encode.encode 2 bodyJson)
            , expect = Http.expectJson userBodyDecoder
            , timeout = Nothing
            , withCredentials = False
            }


sendActivation : Authentication -> Http.Request UserBody
sendActivation authentication =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders <| Just authentication
        , url = apiUrl ++ "users/" ++ authentication.urlName ++ "/send-activation"
        , body = Http.emptyBody
        , expect = Http.expectJson userBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }



-- newTaskCreateStatement : Authentication -> StatementCustom -> Task Http.Error DataIdBody
-- newTaskCreateStatement authentication statementCustom =
--     let
--         bodyJson =
--             Encode.object
--                 ([ ( "type", Encode.string (convertStatementCustomToKind statementCustom) ) ]
--                     ++ case statementCustom of
--                         AbuseCustom abuse ->
--                             [ ( "statementId", Encode.string abuse.statementId )
--                             ]
--                         ArgumentCustom argument ->
--                             [ ( "argumentType", Encode.string (convertArgumentTypeToString argument.argumentType) )
--                             , ( "claimId", Encode.string argument.claimId )
--                             , ( "groundId", Encode.string argument.groundId )
--                             ]
--                         CitationCustom citation ->
--                             [ ( "citedId", Encode.string citation.citedId )
--                             , ( "eventId", Encode.string citation.eventId )
--                             , ( "personId", Encode.string citation.personId )
--                             ]
--                         EventCustom event ->
--                             [ ( "name", Encode.string event.name )
--                             ]
--                         PersonCustom person ->
--                             [ ( "name", Encode.string person.name )
--                             , ( "twitterName", Encode.string person.twitterName )
--                             ]
--                         PlainCustom plain ->
--                             [ ( "languageCode", Encode.string plain.languageCode )
--                             , ( "name", Encode.string plain.name )
--                             ]
--                         TagCustom tag ->
--                             [ ( "name", Encode.string tag.name )
--                             ]
--                 )
--     in
--         Http.fromJson decodeDataIdBody
--             (Http.send Http.defaultSettings
--                 { verb = "POST"
--                 , url =
--                     (apiUrl
--                         ++ "statements"
--                         ++ "?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
--                         ++ "&show=tags"
--                     )
--                 , headers =
--                     [ ( "Accept", "application/json" )
--                     , ( "Content-Type", "application/json" )
--                     , ( "Retruco-API-Key", authentication.apiKey )
--                     ]
--                 , body = Http.string (Encode.encode 2 bodyJson)
--                 }
--             )
-- newTaskDeleteStatementRating : Authentication -> String -> Task Http.Error DataIdBody
-- newTaskDeleteStatementRating authentication statementId =
--     Http.fromJson decodeDataIdBody
--         (Http.send Http.defaultSettings
--             { verb = "DELETE"
--             , url =
--                 (apiUrl
--                     ++ "statements/"
--                     ++ statementId
--                     ++ "/rating?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
--                     ++ "&show=tags"
--                 )
--             , headers =
--                 [ ( "Accept", "application/json" )
--                 , ( "Retruco-API-Key", authentication.apiKey )
--                 ]
--             , body = Http.empty
--             }
--         )
-- newTaskFlagAbuse : Authentication -> String -> Task Http.Error DataIdBody
-- newTaskFlagAbuse authentication statementId =
--     Http.fromJson decodeDataIdBody
--         (Http.send Http.defaultSettings
--             { verb = "GET"
--             , url =
--                 (apiUrl
--                     ++ "statements/"
--                     ++ statementId
--                     ++ "/abuse?depth=1&show=abuse&show=author&show=ballot&show=grounds&show=properties&show=references"
--                     ++ "&show=tags"
--                 )
--             , headers =
--                 [ ( "Accept", "application/json" )
--                 , ( "Retruco-API-Key", authentication.apiKey )
--                 ]
--             , body = Http.empty
--             }
--         )
-- newTaskGetStatements : Maybe Authentication -> SearchCriteria -> Task Http.Error DataIdsBody
-- newTaskGetStatements authentication searchCriteria =
--     let
--         authenticationHeaders =
--             case authentication of
--                 Just authentication ->
--                     [ ( "Retruco-API-Key", authentication.apiKey )
--                     ]
--                 Nothing ->
--                     []
--     in
--         Http.fromJson decodeDataIdsBody
--             (Http.send Http.defaultSettings
--                 { verb = "GET"
--                 , url =
--                     Http.url (apiUrl ++ "statements")
--                         ([ ( "depth", "1" )
--                          , ( "show", "abuse" )
--                          , ( "show", "author" )
--                          , ( "show", "ballot" )
--                          , ( "show", "grounds" )
--                          , ( "show", "properties" )
--                          , ( "show", "tags" )
--                          ]
--                             ++ (case searchCriteria.languageCodeMaybe of
--                                     Just languageCode ->
--                                         [ ( "languageCode", languageCode ) ]
--                                     Nothing ->
--                                         []
--                                )
--                             ++ (case searchCriteria.termMaybe of
--                                     Just term ->
--                                         [ ( "term", term ) ]
--                                     Nothing ->
--                                         []
--                                )
--                             ++ List.map (\kind -> ( "type", kind )) searchCriteria.kinds
--                         )
--                 , headers =
--                     [ ( "Accept", "application/json" )
--                     ]
--                         ++ authenticationHeaders
--                 , body = Http.empty
--                 }
--             )
-- updateFromDataId : DataId -> ModelFragment a -> ModelFragment a
-- updateFromDataId data model =
--     { model
--         | ballotById =
--             Dict.merge
--                 (\id ballot ballotById ->
--                     if ballot.deleted then
--                         ballotById
--                     else
--                         Dict.insert id ballot ballotById
--                 )
--                 (\id leftBallot rightBallot ballotById ->
--                     if leftBallot.deleted then
--                         ballotById
--                     else
--                         Dict.insert id leftBallot ballotById
--                 )
--                 Dict.insert
--                 data.ballots
--                 model.ballotById
--                 Dict.empty
--         , statementById =
--             Dict.merge
--                 (\id statement statementById ->
--                     if statement.deleted then
--                         statementById
--                     else
--                         Dict.insert id statement statementById
--                 )
--                 (\id leftStatement rightStatement statementById ->
--                     if leftStatement.deleted then
--                         statementById
--                     else
--                         Dict.insert id leftStatement statementById
--                 )
--                 Dict.insert
--                 data.statements
--                 model.statementById
--                 Dict.empty
--         , statementIds =
--             if Dict.member data.id data.statements then
--                 if List.member data.id model.statementIds then
--                     model.statementIds
--                 else
--                     data.id :: model.statementIds
--             else
--                 -- data.id is not the ID of a statement (but a ballot ID, etc).
--                 model.statementIds
--     }
