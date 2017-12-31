module Decoders exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))
import String
import Types exposing (..)


argumentDecoder : Decoder Argument
argumentDecoder =
    succeed Argument
        |: field "id" string
        |: field "keyId" string
        |: oneOf [ field "ratingCount" int, succeed 0 ]
        |: oneOf [ field "ratingSum" int, succeed 0 ]
        |: field "valueId" string


ballotDecoder : Decoder Ballot
ballotDecoder =
    succeed Ballot
        |: oneOf [ field "rating" int |> andThen (\_ -> succeed False), succeed True ]
        |: field "id" string
        |: oneOf [ field "rating" int, succeed 0 ]
        |: field "statementId" string
        |: oneOf [ field "updatedAt" string, succeed "" ]
        |: field "voterId" string


cardAutocompletionDecoder : Decoder CardAutocompletion
cardAutocompletionDecoder =
    succeed CardAutocompletion
        |: field "autocomplete" string
        |: field "card" cardDecoder
        |: field "distance" float


cardDecoder : Decoder Card
cardDecoder =
    succeed Card
        |: oneOf [ field "argumentCount" int, succeed 0 ]
        |: oneOf [ field "ballotId" string, succeed "" ]
        |: field "createdAt" string
        |: oneOf [ field "symbol" string, field "id" string ]
        |: oneOf [ field "qualities" qualitiesDecoder, succeed Dict.empty ]
        |: oneOf [ field "ratingCount" int, succeed 0 ]
        |: oneOf [ field "ratingSum" int, succeed 0 ]
        |: oneOf [ field "references" (dict (list string)), succeed Dict.empty ]
        |: oneOf [ field "subTypeIds" (list string), succeed [] ]
        |: oneOf [ field "tagIds" (list string), succeed [] ]
        |: oneOf [ field "trashed" bool, succeed False ]
        |: field "type" string
        |: oneOf [ field "usageIds" (list string), succeed [] ]


cardsAutocompletionBodyDecoder : Decoder CardsAutocompletionBody
cardsAutocompletionBodyDecoder =
    succeed CardsAutocompletionBody
        |: field "data" (list cardAutocompletionDecoder)


collectionDecoder : Decoder Collection
collectionDecoder =
    succeed Collection
        |: field "authorId" string
        |: oneOf [ field "cardIds" (list string), succeed [] ]
        |: oneOf [ field "description" string, succeed "" ]
        |: field "id" string
        |: (maybe (field "logo" string)
                |> map
                    (\v ->
                        v
                            |> Maybe.andThen
                                (\s ->
                                    if String.isEmpty s then
                                        Nothing
                                    else
                                        Just s
                                )
                    )
           )
        |: field "name" string


dataWithIdBodyDecoder : Decoder DataWithIdBody
dataWithIdBodyDecoder =
    succeed DataWithIdBody
        |: field "data" dataWithIdDecoder


dataWithIdDecoder : Decoder DataWithId
dataWithIdDecoder =
    succeed DataWithId
        |: oneOf [ field "ballots" (dict ballotDecoder), succeed Dict.empty ]
        |: oneOf [ field "cards" (dict cardDecoder), succeed Dict.empty ]
        |: oneOf [ field "collections" (dict collectionDecoder), succeed Dict.empty ]
        |: field "id" string
        |: oneOf [ field "properties" (dict propertyDecoder), succeed Dict.empty ]
        |: oneOf [ field "users" (dict userDecoder), succeed Dict.empty ]
        |: oneOf [ field "values" (dict typedValueDecoder), succeed Dict.empty ]


dataWithIdsBodyDecoder : Decoder DataWithIdsBody
dataWithIdsBodyDecoder =
    succeed DataWithIdsBody
        |: oneOf [ field "count" int, succeed 0 ]
        |: field "data" dataWithIdsDecoder
        |: oneOf [ field "limit" int, succeed 0 ]
        |: oneOf [ field "offset" int, succeed 0 ]


dataWithIdsDecoder : Decoder DataWithIds
dataWithIdsDecoder =
    map2 (,)
        (field "ids" (array string))
        (oneOf [ field "users" (dict userDecoder), succeed Dict.empty ])
        |> andThen
            (\( ids, users ) ->
                (if Array.isEmpty ids then
                    succeed ( Dict.empty, Dict.empty, Dict.empty, Dict.empty, Dict.empty )
                 else
                    map5 (,,,,)
                        (oneOf [ field "ballots" (dict ballotDecoder), succeed Dict.empty ])
                        (oneOf [ field "cards" (dict cardDecoder), succeed Dict.empty ])
                        (oneOf [ field "collections" (dict collectionDecoder), succeed Dict.empty ])
                        (oneOf [ field "properties" (dict propertyDecoder), succeed Dict.empty ])
                        (oneOf [ field "values" (dict typedValueDecoder), succeed Dict.empty ])
                )
                    |> map
                        (\( ballots, cards, collections, properties, values ) ->
                            DataWithIds ballots cards collections ids properties users values
                        )
            )


graphqlDataWithIdDecoder : Decoder DataWithId
graphqlDataWithIdDecoder =
    map7
        (\ballots cards collections id properties users values ->
            let
                objectsListToDict objects =
                    objects
                        |> List.map (\object -> ( object.id, object ))
                        |> Dict.fromList
            in
                { ballots = objectsListToDict ballots
                , cards = objectsListToDict cards
                , collections = objectsListToDict collections
                , id = id
                , properties = objectsListToDict properties
                , users = objectsListToDict users
                , values = objectsListToDict values
                }
        )
        (oneOf [ field "ballots" (list ballotDecoder), succeed [] ])
        (oneOf [ field "cards" (list cardDecoder), succeed [] ])
        (oneOf [ field "collections" (list collectionDecoder), succeed [] ])
        (field "id" string)
        (oneOf [ field "properties" (list propertyDecoder), succeed [] ])
        (oneOf [ field "users" (list userDecoder), succeed [] ])
        (oneOf [ field "values" (list typedValueDecoder), succeed [] ])


graphqlPropertyDecoder : Decoder DataWithId
graphqlPropertyDecoder =
    map2
        (\property typedValue ->
            { initDataWithId
                | id = property.id
                , properties = Dict.singleton property.id property
            }
                |> addToData typedValue
        )
        propertyDecoder
        (field "value" statementWrapperDecoder)


messageBodyDecoder : Decoder String
messageBodyDecoder =
    (field "data" string)


popularTagDecoder : Decoder PopularTag
popularTagDecoder =
    succeed PopularTag
        |: field "count" float
        |: field "tagId" string


popularTagsDataDecoder : Decoder PopularTagsData
popularTagsDataDecoder =
    (field "data"
        (succeed PopularTagsData
            |: field "popularity" (list popularTagDecoder)
            |: oneOf [ field "values" (dict typedValueDecoder), succeed Dict.empty ]
        )
    )


propertyDecoder : Decoder Property
propertyDecoder =
    succeed Property
        |: oneOf [ field "argumentCount" int, succeed 0 ]
        |: oneOf [ field "ballotId" string, succeed "" ]
        |: field "createdAt" string
        |: oneOf [ field "symbol" string, field "id" string ]
        |: field "keyId" string
        |: field "objectId" string
        |: oneOf [ field "qualities" qualitiesDecoder, succeed Dict.empty ]
        |: oneOf [ field "ratingCount" int, succeed 0 ]
        |: oneOf [ field "ratingSum" int, succeed 0 ]
        |: oneOf [ field "references" (dict (list string)), succeed Dict.empty ]
        |: oneOf [ field "subTypeIds" (list string), succeed [] ]
        |: oneOf [ field "tags" (list (dict string)), succeed [] ]
        |: oneOf [ field "trashed" bool, succeed False ]
        |: field "type" string
        |: field "valueId" string


qualitiesDecoder : Decoder (Dict String (List String))
qualitiesDecoder =
    oneOf
        [ dict (list string)
        , list qualityItemDecoder |> andThen (succeed << Dict.fromList)
        ]


qualityItemDecoder : Decoder ( String, List String )
qualityItemDecoder =
    succeed (,)
        |: field "keyId" string
        |: field "valueIds" (list string)


statementWrapperDecoder : Decoder StatementWrapper
statementWrapperDecoder =
    field "type" string
        |> andThen statementWrapperDecoderFromType


statementWrapperDecoderFromType : String -> Decoder StatementWrapper
statementWrapperDecoderFromType type_ =
    case type_ of
        "Card" ->
            cardDecoder
                |> andThen (succeed << CardWrapper)

        "Property" ->
            propertyDecoder
                |> andThen (succeed << PropertyWrapper)

        "Value" ->
            typedValueDecoder
                |> andThen (succeed << TypedValueWrapper)

        _ ->
            fail <|
                "Trying to decode StatementWrapper, but type "
                    ++ toString type_
                    ++ " is not supported."


typedValueAutocompletionDecoder : Decoder TypedValueAutocompletion
typedValueAutocompletionDecoder =
    succeed TypedValueAutocompletion
        |: field "autocomplete" string
        |: field "distance" float
        |: field "value" typedValueDecoder


typedValueDecoder : Decoder TypedValue
typedValueDecoder =
    succeed
        (\argumentCount ballotId createdAt id qualities ratingCount ratingSum schemaId trashed type_ widgetId ->
            { argumentCount = argumentCount
            , ballotId = ballotId
            , createdAt = createdAt
            , id = id
            , qualities = qualities
            , ratingCount = ratingCount
            , ratingSum = ratingSum
            , schemaId = schemaId
            , trashed = trashed
            , type_ = type_
            , widgetId = widgetId
            }
        )
        |: oneOf [ field "argumentCount" int, succeed 0 ]
        |: oneOf [ field "ballotId" string, succeed "" ]
        |: field "createdAt" string
        |: oneOf [ field "symbol" string, field "id" string ]
        |: oneOf [ field "qualities" qualitiesDecoder, succeed Dict.empty ]
        |: oneOf [ field "ratingCount" int, succeed 0 ]
        |: oneOf [ field "ratingSum" int, succeed 0 ]
        |: field "schemaId" string
        |: oneOf [ field "trashed" bool, succeed False ]
        |: field "type" string
        |: oneOf [ field "widgetId" string, succeed "" ]
        |> andThen
            (\{ argumentCount, ballotId, createdAt, id, qualities, ratingCount, ratingSum, schemaId, trashed, type_, widgetId } ->
                (field "value" (valueTypeDecoder schemaId widgetId))
                    |> map
                        (\value ->
                            TypedValue
                                argumentCount
                                ballotId
                                createdAt
                                id
                                qualities
                                ratingCount
                                ratingSum
                                schemaId
                                trashed
                                type_
                                value
                                widgetId
                        )
            )


typedValuesAutocompletionBodyDecoder : Decoder TypedValuesAutocompletionBody
typedValuesAutocompletionBodyDecoder =
    succeed TypedValuesAutocompletionBody
        |: field "data" (list typedValueAutocompletionDecoder)


userBodyDecoder : Decoder UserBody
userBodyDecoder =
    succeed UserBody
        |: field "data" userDecoder


userDecoder : Decoder User
userDecoder =
    succeed User
        |: field "activated" bool
        |: oneOf [ field "apiKey" string, succeed "" ]
        |: oneOf [ field "email" string, succeed "" ]
        |: field "id" string
        |: field "isAdmin" bool
        |: field "name" string
        |: field "urlName" string


valueTypeDecoder : String -> String -> Decoder ValueType
valueTypeDecoder schemaId widgetId =
    let
        decoder =
            case ( schemaId, widgetId ) of
                ( "schema:boolean", _ ) ->
                    bool |> map BooleanValue

                ( "schema:email", _ ) ->
                    string |> map EmailValue

                ( "schema:ids-array", _ ) ->
                    list string |> map IdsArrayValue

                ( "schema:number", _ ) ->
                    float |> map NumberValue

                ( "schema:string", _ ) ->
                    string |> map StringValue

                ( "schema:uri", "widget:image" ) ->
                    string |> map ImagePathValue

                ( "schema:uri", _ ) ->
                    string |> map UrlValue

                ( "schema:uri-reference", "widget:image" ) ->
                    string |> map ImagePathValue

                ( "schema:uri-reference", _ ) ->
                    string |> map UrlValue

                ( _, _ ) ->
                    fail ("TODO Unsupported schemaId \"" ++ schemaId ++ "\" & widgetId \"" ++ widgetId ++ "\"")
    in
        oneOf
            [ decoder
            , value
                |> map
                    (\value ->
                        let
                            str =
                                toString value

                            -- _ =
                            --     Debug.log ("WrongValue \"" ++ str ++ "\", schemaId: " ++ schemaId)
                        in
                            WrongValue str schemaId
                    )
            ]
