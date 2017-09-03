module Requests exposing (..)

import Authenticator.Types exposing (Authentication)
import Configuration exposing (apiUrl)
import Decoders exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Types exposing (..)
import Urls


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
    -> List String
    -> String
    -> Int
    -> Http.Request CardsAutocompletionBody
autocompleteCards authentication language cardTypes term limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "cards/autocomplete"
                ++ Urls.paramsToQuery
                    ([ ( "language", Just (I18n.languageIdFromLanguage language) )
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
                     ]
                        ++ List.map (\cardType -> ( "type", Just cardType )) cardTypes
                    )
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


autocompletePropertiesKeys :
    Maybe Authentication
    -> I18n.Language
    -> List String
    -> String
    -> Int
    -> Http.Request TypedValuesAutocompletionBody
autocompletePropertiesKeys authentication language cardTypes term limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "properties/keys/autocomplete"
                ++ Urls.paramsToQuery
                    ([ ( "class", Just "Card" )
                     , ( "language", Just (I18n.languageIdFromLanguage language) )
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
                     ]
                        ++ List.map (\cardType -> ( "type", Just cardType )) cardTypes
                    )
        , body = Http.emptyBody
        , expect = Http.expectJson typedValuesAutocompletionBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


autocompleteValues :
    Maybe Authentication
    -> I18n.Language
    -> List String
    -> List String
    -> String
    -> Int
    -> Http.Request TypedValuesAutocompletionBody
autocompleteValues authentication language schemas widgets term limit =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "values/autocomplete"
                ++ Urls.paramsToQuery
                    ([ ( "language", Just (I18n.languageIdFromLanguage language) )
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
                     ]
                        ++ List.map (\schema -> ( "schema", Just schema )) schemas
                        ++ List.map (\widget -> ( "widget", Just widget )) widgets
                    )
        , body = Http.emptyBody
        , expect = Http.expectJson typedValuesAutocompletionBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCard : Maybe Authentication -> String -> Http.Request DataIdBody
getCard authentication cardId =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "objects/" ++ cardId ++ "?depth=5&show=references&show=values"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getCards :
    Maybe Authentication
    -> String
    -> Int
    -> Int
    -> List String
    -> List String
    -> Bool
    -> Http.Request DataIdsBody
getCards authentication term limit offset tagIds cardTypes showTrashed =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "cards"
                ++ Urls.paramsToQuery
                    ([ ( "depth", Just "2" )
                     , ( "limit", Just (toString limit) )
                     , ( "offset", Just (toString offset) )
                     , ( "show"
                       , if showTrashed then
                            Just "trashed"
                         else
                            Nothing
                       )
                     , ( "show", Just "values" )
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
                     ]
                        ++ List.map
                            (\tagId ->
                                ( "tag"
                                , let
                                    cleanTagId =
                                        String.trim tagId
                                  in
                                    if String.isEmpty cleanTagId then
                                        Nothing
                                    else
                                        Just cleanTagId
                                )
                            )
                            tagIds
                        ++ (cardTypes
                                |> List.map (\cardType -> ( "type", Just cardType ))
                           )
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
        , url = apiUrl ++ "collections/" ++ collectionId ++ "?show=values&depth=4"
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
                ++ "collections"
                ++ Urls.paramsToQuery
                    [ ( "depth", Just "1" )
                    , ( "limit"
                      , case limit of
                            Just limit ->
                                Just (toString limit)

                            Nothing ->
                                Nothing
                      )
                    , ( "show", Just "values" )
                    ]
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


getProperties : Maybe Authentication -> Bool -> List String -> List String -> List String -> Http.Request DataIdsBody
getProperties authentication showTrashed objectIds keyIds valueIds =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "properties"
                ++ Urls.paramsToQuery
                    ([ ( "depth", Just "3" )
                     , ( "show", Just "ballots" )
                     , ( "show"
                       , if showTrashed then
                            Just "trashed"
                         else
                            Nothing
                       )
                     , ( "show", Just "values" )
                     ]
                        ++ (List.map (\keyId -> ( "keyId", Just keyId )) keyIds)
                        ++ (List.map (\objectId -> ( "objectId", Just objectId )) objectIds)
                        ++ (List.map (\valueId -> ( "valueId", Just valueId )) valueIds)
                    )
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
                ++ "cards/tags-popularity"
                ++ Urls.paramsToQuery
                    (List.map
                        (\tagId ->
                            ( "tag"
                            , let
                                cleanTagId =
                                    String.trim tagId
                              in
                                if String.isEmpty cleanTagId then
                                    Nothing
                                else
                                    Just cleanTagId
                            )
                        )
                        tagIds
                        ++ [ ( "type", Just "use-case" ) ]
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
        , url = apiUrl ++ "objects/" ++ id ++ "?depth=3&show=ballots&show=properties&show=values"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getValues : Maybe Authentication -> Maybe String -> Maybe Int -> Bool -> Bool -> Http.Request DataIdsBody
getValues authentication term limit ratedOnly showTrashed =
    Http.request
        { method = "GET"
        , headers = authenticationHeaders authentication
        , url =
            apiUrl
                ++ "values"
                ++ Urls.paramsToQuery
                    ([ ( "depth", Just "3" )
                     , ( "limit"
                       , case limit of
                            Just limit ->
                                Just (toString limit)

                            Nothing ->
                                Nothing
                       )
                     , ( "rated"
                       , if ratedOnly then
                            Just "true"
                         else
                            Nothing
                       )
                     , ( "show", Just "ballots" )
                     , ( "show", Just "properties" )
                     , ( "show"
                       , if showTrashed then
                            Just "trashed"
                         else
                            Nothing
                       )
                     , ( "show", Just "values" )
                     , ( "term"
                       , case term of
                            Just term ->
                                let
                                    cleanTerm =
                                        String.trim term
                                in
                                    if String.isEmpty cleanTerm then
                                        Nothing
                                    else
                                        Just cleanTerm

                            Nothing ->
                                Nothing
                       )
                     ]
                    )
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdsBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


postCard : Maybe Authentication -> Dict String String -> I18n.Language -> Http.Request DataIdBody
postCard authentication fields language =
    let
        -- languageCode =
        --     I18n.languageIdFromLanguage language
        body =
            Encode.object
                -- Always use en(glish) language because this is the language of the labels below.
                -- [ ( "language", Encode.string languageCode )
                [ ( "language", Encode.string "en" )
                , ( "schemas"
                  , Encode.object
                        [ ( "Description", Encode.string "schema:string" )
                        , ( "Download", Encode.string "schema:uri" )
                        , ( "Logo", Encode.string "schema:uri" )
                        , ( "Name", Encode.string "schema:string" )
                        , ( "Types", Encode.string "schema:value-id" )
                        , ( "Website", Encode.string "schema:uri" )
                        ]
                  )
                , ( "values"
                  , [ ( "Description", Encode.string )
                    , ( "Download", Encode.string )
                    , ( "Logo", Encode.string )
                    , ( "Name", Encode.string )
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
            , url = apiUrl ++ "cards/easy" ++ "?depth=5&show=references&show=values"
            , body = body
            , expect = Http.expectJson dataIdBodyDecoder
            , timeout = Nothing
            , withCredentials = False
            }


postCollection : Maybe Authentication -> Maybe String -> Encode.Value -> Http.Request DataIdBody
postCollection authentication collectionId collectionJson =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url =
            case collectionId of
                Just collectionId ->
                    apiUrl ++ "collections/" ++ collectionId

                Nothing ->
                    apiUrl ++ "collections"
        , body = Http.jsonBody collectionJson
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }


postProperty : Maybe Authentication -> String -> String -> String -> Maybe Int -> Http.Request DataIdBody
postProperty authentication objectId keyId valueId rating =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "properties?show=ballots&show=values&depth=3"
        , body =
            Encode.object
                ([ ( "keyId", Just <| Encode.string keyId )
                 , ( "objectId", Just <| Encode.string objectId )
                 , ( "rating"
                   , case rating of
                        Just rating ->
                            Just <| Encode.int rating

                        Nothing ->
                            Nothing
                   )
                 , ( "valueId", Just <| Encode.string valueId )
                 ]
                    |> List.filterMap
                        (\( key, value ) ->
                            case value of
                                Just value ->
                                    Just ( key, value )

                                Nothing ->
                                    Nothing
                        )
                )
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


postValue : Authentication -> Field -> Http.Request DataIdBody
postValue authentication field =
    let
        ( schemaId, widgetId, encodedValue ) =
            case field of
                BooleanField bool ->
                    ( "schema:boolean", "widget:input-checkbox", Encode.bool bool )

                CardIdField string ->
                    case Urls.urlToId string of
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

                InputTextField _ string ->
                    ( "schema:string", "widget:input-text", Encode.string string )

                InputUrlField string ->
                    ( "schema:uri", "widget:input-url", Encode.string string )

                TextareaField _ string ->
                    ( "schema:string", "widget:textarea", Encode.string string )

                ValueIdField string ->
                    case Urls.urlToId string of
                        Just valueId ->
                            ( "schema:value-id", "widget:autocomplete", Encode.string valueId )

                        Nothing ->
                            -- TODO: Improve errors handling.
                            ( "schema:string", "widget:input-text", Encode.string string )
    in
        Http.request
            { method = "POST"
            , headers = authenticationHeaders (Just authentication)
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


rateStatement : Maybe Authentication -> String -> Int -> Http.Request DataIdBody
rateStatement authentication statementId rating =
    Http.request
        { method = "POST"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "statements/" ++ statementId ++ "/rating?show=ballots&depth=1"
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


unrateStatement : Maybe Authentication -> String -> Http.Request DataIdBody
unrateStatement authentication statementId =
    Http.request
        { method = "DELETE"
        , headers = authenticationHeaders authentication
        , url = apiUrl ++ "statements/" ++ statementId ++ "/rating?show=ballots&depth=1"
        , body = Http.emptyBody
        , expect = Http.expectJson dataIdBodyDecoder
        , timeout = Nothing
        , withCredentials = False
        }
