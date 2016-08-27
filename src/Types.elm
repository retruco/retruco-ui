module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing ((:=), andThen, bool, Decoder, dict, fail, int, list, map, maybe, null, oneOf, string,
    succeed)
import Json.Decode.Extra as Json exposing ((|:))


type alias Abuse =
    {}


type alias Argument =
    {}


type alias Ballot =
    { deleted : Bool
    , id : String
    , rating : Int
    , statementId : String
    , updatedAt : String
    , voterId : String
    }


type alias DataId =
    { ballots : Dict String Ballot
    , id : String
    , statements : Dict String Statement
    , users : Dict String User
    }


type alias DataIdBody =
    { data : DataId
    }


type alias DataIds =
    { ballots : Dict String Ballot
    , ids : List String
    , statements : Dict String Statement
    , users : Dict String User
    }


type alias DataIdsBody =
    { data : DataIds
    }


type alias Plain =
    { name : Maybe String
    }


type alias Statement =
    { createdAt : String
    , custom : StatementCustom
    , deleted : Bool
    , id : String
    }


type StatementCustom
    = AbuseCustom Abuse
    | ArgumentCustom Argument
    | PlainCustom Plain
    | TagCustom Tag


type alias Tag =
    {}


type alias User =
    { apiKey : String
    , email : String
    , name : String
    , urlName : String
    }


type alias UserBody =
    { data : User
    }


decodeBallot : Decoder Ballot
decodeBallot =
    succeed Ballot
        |: ("deleted" := bool)
        |: ("id" := string)
        |: ("rating" := int)
        |: ("statementId" := string)
        |: ("updatedAt" := string)
        |: ("voterId" := string)


decodeDataId : Decoder DataId
decodeDataId =
    succeed DataId
        |: oneOf [("ballots" := dict decodeBallot), null Dict.empty, succeed Dict.empty]
        |: ("id" := string)
        |: oneOf [("statements" := dict decodeStatement), null Dict.empty, succeed Dict.empty]
        |: oneOf [("users" := dict decodeUser), null Dict.empty, succeed Dict.empty]


decodeDataIds : Decoder DataIds
decodeDataIds =
    succeed DataIds
        |: oneOf [("ballots" := dict decodeBallot), null Dict.empty, succeed Dict.empty]
        |: ("ids" := list string)
        |: oneOf [("statements" := dict decodeStatement), null Dict.empty, succeed Dict.empty]
        |: oneOf [("users" := dict decodeUser), null Dict.empty, succeed Dict.empty]


decodeDataIdBody : Decoder DataIdBody
decodeDataIdBody =
    succeed DataIdBody
        |: ("data" := decodeDataId)


decodeDataIdsBody : Decoder DataIdsBody
decodeDataIdsBody =
    succeed DataIdsBody
        |: ("data" := decodeDataIds)


decodeStatement : Decoder Statement
decodeStatement =
    succeed Statement
        |: ("createdAt" := string)
        |: (("type" := string) `andThen` decodeStatementFromType)
        |: ("deleted" := bool)
        |: ("id" := string)


decodeStatementFromType : String -> Decoder StatementCustom
decodeStatementFromType statementType =
    case statementType of
        "Abuse" ->
            succeed (AbuseCustom {})

        "Argument" ->
            succeed (ArgumentCustom {})

        "PlainStatement" ->
            succeed Plain
                |: maybe ("name" := string)
            `andThen` \plain -> succeed (PlainCustom plain)

        "Tag" ->
            succeed (TagCustom {})

        _ ->
            fail ("Unkown statement type: " ++ statementType)


decodeUser : Decoder User
decodeUser =
    succeed User
        |: ("apiKey" := string)
        |: ("email" := string)
        |: ("name" := string)
        |: ("urlName" := string)


decodeUserBody : Decoder UserBody
decodeUserBody =
    succeed UserBody
        |: ("data" := decodeUser)
