module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing ((:=), andThen, bool, Decoder, dict, fail, int, list, map, maybe, null, oneOf, string,
    succeed)
import Json.Decode.Extra as Json exposing ((|:))


type alias Abuse =
    { statementId : String
    }


type alias Argument =
    { claimId : String
    , groundId : String
    }


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


type alias FormErrors = Dict String String


type alias Plain =
    { languageCode : String
    , name : String
    }


type alias Statement =
    { ballotIdMaybe : Maybe String
    , createdAt : String
    , custom : StatementCustom
    , deleted : Bool
    , groundIds : List String
    , id : String
    }


type StatementCustom
    = AbuseCustom Abuse
    | ArgumentCustom Argument
    | PlainCustom Plain
    | TagCustom Tag


type alias StatementForm =
    { claimId : String
    , errors : FormErrors
    , groundId : String
    , kind : String
    , languageCode : String
    , name : String
    , statementId : String
    }


type alias Tag =
    { name : String
    }


type alias User =
    { apiKey : String
    , email : String
    , name : String
    , urlName : String
    }


type alias UserBody =
    { data : User
    }


convertStatementCustomToKind : StatementCustom -> String
convertStatementCustomToKind statementCustom =
    case statementCustom of
        AbuseCustom abuse ->
            "Abuse"

        ArgumentCustom argument ->
            "Argument"

        PlainCustom plain ->
            "PlainStatement"

        TagCustom tag ->
            "Tag"


convertStatementFormToCustom : StatementForm -> StatementCustom
convertStatementFormToCustom form =
    case form.kind of
        "Abuse" ->
            AbuseCustom
                { statementId = form.statementId
                }

        "Argument" ->
            ArgumentCustom
                { claimId = form.claimId
                , groundId = form.groundId
                }

        "PlainStatement" ->
            PlainCustom
                { languageCode = form.languageCode
                , name = form.name
                }

        "Tag" ->
            TagCustom
                { name = form.name
                }

        _ ->
            -- TODO: Return a Result instead of a dummy PlainCustom.
            PlainCustom
                { languageCode = "en"
                , name = "Unknown kind: " ++ form.kind
                }


decodeBallot : Decoder Ballot
decodeBallot =
    succeed Ballot
        |: oneOf [("deleted" := bool), succeed False]
        |: ("id" := string)
        |: ("rating" := int)
        |: ("statementId" := string)
        |: ("updatedAt" := string)
        |: ("voterId" := string)


decodeDataId : Decoder DataId
decodeDataId =
    succeed DataId
        |: oneOf [("ballots" := dict decodeBallot), succeed Dict.empty]
        |: ("id" := string)
        |: oneOf [("statements" := dict decodeStatement), succeed Dict.empty]
        |: oneOf [("users" := dict decodeUser), succeed Dict.empty]


decodeDataIds : Decoder DataIds
decodeDataIds =
    succeed DataIds
        |: oneOf [("ballots" := dict decodeBallot), succeed Dict.empty]
        |: ("ids" := list string)
        |: oneOf [("statements" := dict decodeStatement), succeed Dict.empty]
        |: oneOf [("users" := dict decodeUser), succeed Dict.empty]


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
        |: maybe ("ballotId" := string)
        |: ("createdAt" := string)
        |: (("type" := string) `andThen` decodeStatementFromType)
        |: oneOf [("deleted" := bool), succeed False]
        |: oneOf [("groundIds" := list string), succeed []]
        |: ("id" := string)


decodeStatementFromType : String -> Decoder StatementCustom
decodeStatementFromType statementType =
    case statementType of
        "Abuse" ->
            succeed Abuse
                |: ("statementId" := string)
            `andThen` \abuse -> succeed (AbuseCustom abuse)

        "Argument" ->
            succeed Argument
                |: ("claimId" := string)
                |: ("groundId" := string)
            `andThen` \argument -> succeed (ArgumentCustom argument)

        "PlainStatement" ->
            succeed Plain
                |: ("languageCode" := string)
                |: ("name" := string)
            `andThen` \plain -> succeed (PlainCustom plain)

        "Tag" ->
            succeed Tag
                |: ("name" := string)
            `andThen` \tag -> succeed (TagCustom tag)

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


initStatementForm : StatementForm
initStatementForm =
    { claimId = ""
    , errors = Dict.empty
    , groundId = ""
    , kind = "PlainStatement"
    , languageCode = "en"
    , name = ""
    , statementId = ""
    }
