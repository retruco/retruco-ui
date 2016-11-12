module Types exposing (..)

import Autocomplete
import Dict exposing (Dict)
import Json.Decode
    exposing
        ( (:=)
        , andThen
        , bool
        , customDecoder
        , Decoder
        , dict
        , fail
        , float
        , int
        , list
        , map
        , maybe
        , null
        , oneOf
        , string
        , succeed
        )
import Json.Decode.Extra as Json exposing ((|:))
import String


type alias Abuse =
    { statementId : String
    }


type alias Argument =
    { argumentType : ArgumentType
    , claimId : String
    , groundId : String
    }


type ArgumentType
    = Because
    | But
    | Comment
    | Example


type AutocompleteMenuState
    = AutocompleteMenuHidden
    | AutocompleteMenuSleeping
    | AutocompleteMenuLoading
    | AutocompleteMenuVisible


type alias Ballot =
    { deleted : Bool
    , id : String
    , rating : Int
    , statementId : String
    , updatedAt : String
    , voterId : String
    }


type alias Citation =
    { citedId : String
    , eventId : String
    , personId : String
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


type alias Event =
    { name : String
    }


type alias EventForm =
    Event


type alias ModelFragment a =
    { a
        | ballotById : Dict String Ballot
        , statementById : Dict String Statement
        , statementIds : List String
    }


type alias FormErrors =
    Dict String String


type alias Person =
    { name : String
    , twitterName : String
    }


type alias PersonForm =
    Person


type alias Plain =
    { languageCode : String
    , name : String
    }


type alias PlainForm =
    Plain


type alias SearchCriteria =
    { kinds : List String
    , languageCodeMaybe : Maybe String
    , sort : String
    , termMaybe : Maybe String
    }


type alias Statement =
    { ballotIdMaybe : Maybe String
    , createdAt : String
    , custom : StatementCustom
    , deleted : Bool
    , groundIds : List String
    , id : String
    , isAbuse : Bool
    , ratingCount : Int
    , ratingSum : Int
    }


type alias StatementAutocompletion =
    { autocomplete : String
    , distance : Float
    , statement : Statement
    }


type StatementCustom
    = AbuseCustom Abuse
    | ArgumentCustom Argument
    | CitationCustom Citation
    | EventCustom Event
    | PersonCustom Person
    | PlainCustom Plain
    | TagCustom Tag


type alias StatementEmbed =
    { autocomplete : String
    , autocompleteMenuState : AutocompleteMenuState
    , autocompleter : Autocomplete.State
    , autocompletions : List StatementAutocompletion
    , editedMaybe : Maybe PersonForm
    , selectedMaybe : Maybe StatementAutocompletion
    }


type alias StatementForm =
    { argumentType : String
    , cited : StatementEmbed
    , citedId : String
    , claimId : String
    , errors : FormErrors
    , event : StatementEmbed
    , eventId : String
    , groundId : String
    , kind : String
    , languageCode : String
    , name : String
    , person : StatementEmbed
    , personId : String
    , statementId : String
    , twitterName : String
    }


type alias StatementsAutocompletionBody =
    { data : List StatementAutocompletion
    }


type alias Tag =
    { name : String
    , statementId : String
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


convertArgumentTypeToString : ArgumentType -> String
convertArgumentTypeToString argumentType =
    case argumentType of
        Because ->
            "because"

        But ->
            "but"

        Comment ->
            "comment"

        Example ->
            "example"


convertStatementCustomToKind : StatementCustom -> String
convertStatementCustomToKind statementCustom =
    case statementCustom of
        AbuseCustom abuse ->
            "Abuse"

        ArgumentCustom argument ->
            "Argument"

        CitationCustom plain ->
            "Citation"

        EventCustom plain ->
            "Event"

        PersonCustom plain ->
            "Person"

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
                { argumentType =
                    case form.argumentType of
                        "because" ->
                            Because

                        "but" ->
                            But

                        "comment" ->
                            Comment

                        "example" ->
                            Example

                        _ ->
                            Comment
                , claimId = form.claimId
                , groundId = form.groundId
                }

        "Citation" ->
            CitationCustom
                { citedId = form.citedId
                , eventId = form.eventId
                , personId = form.personId
                }

        "Event" ->
            EventCustom
                { name = form.name
                }

        "Person" ->
            PersonCustom
                { name = form.name
                , twitterName = form.twitterName
                }

        "PlainStatement" ->
            PlainCustom
                { languageCode = form.languageCode
                , name = form.name
                }

        "Tag" ->
            TagCustom
                { name = form.name
                , statementId = form.statementId
                }

        _ ->
            -- TODO: Return a Result instead of a dummy PlainCustom.
            PlainCustom
                { languageCode = "en"
                , name = "Unknown kind: " ++ form.kind
                }


decodeArgumentType : Decoder ArgumentType
decodeArgumentType =
    customDecoder string
        (\argumentType ->
            case argumentType of
                "because" ->
                    Ok Because

                "but" ->
                    Ok But

                "comment" ->
                    Ok Comment

                "example" ->
                    Ok Example

                _ ->
                    Err ("Unkown argument type: " ++ argumentType)
        )


decodeBallot : Decoder Ballot
decodeBallot =
    succeed Ballot
        |: oneOf [ ("rating" := int) `andThen` (\_ -> succeed False), succeed True ]
        |: ("id" := string)
        |: oneOf [ ("rating" := int), succeed 0 ]
        |: ("statementId" := string)
        |: oneOf [ ("updatedAt" := string), succeed "" ]
        |: ("voterId" := string)


decodeDataId : Decoder DataId
decodeDataId =
    succeed DataId
        |: oneOf [ ("ballots" := dict decodeBallot), succeed Dict.empty ]
        |: ("id" := string)
        |: oneOf [ ("statements" := dict decodeStatement), succeed Dict.empty ]
        |: oneOf [ ("users" := dict decodeUser), succeed Dict.empty ]


decodeDataIds : Decoder DataIds
decodeDataIds =
    succeed DataIds
        |: oneOf [ ("ballots" := dict decodeBallot), succeed Dict.empty ]
        |: ("ids" := list string)
        |: oneOf [ ("statements" := dict decodeStatement), succeed Dict.empty ]
        |: oneOf [ ("users" := dict decodeUser), succeed Dict.empty ]


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
        |: oneOf [ ("deleted" := bool), succeed False ]
        |: oneOf [ ("groundIds" := list string), succeed [] ]
        |: ("id" := string)
        |: oneOf [ ("isAbuse" := bool), succeed False ]
        |: oneOf [ ("ratingCount" := int), succeed 0 ]
        |: oneOf [ ("ratingSum" := int), succeed 0 ]


decodeStatementAutocompletion : Decoder StatementAutocompletion
decodeStatementAutocompletion =
    succeed StatementAutocompletion
        |: ("autocomplete" := string)
        |: ("distance" := float)
        |: ("statement" := decodeStatement)


decodeStatementFromType : String -> Decoder StatementCustom
decodeStatementFromType statementType =
    case statementType of
        "Abuse" ->
            succeed Abuse
                |: ("statementId" := string)
                `andThen` \abuse -> succeed (AbuseCustom abuse)

        "Argument" ->
            succeed Argument
                |: ("argumentType" := decodeArgumentType)
                |: ("claimId" := string)
                |: ("groundId" := string)
                `andThen` \argument -> succeed (ArgumentCustom argument)

        "Citation" ->
            succeed Citation
                |: ("citedId" := string)
                |: ("eventId" := string)
                |: ("personId" := string)
                `andThen` \citation -> succeed (CitationCustom citation)

        "Event" ->
            succeed Event
                |: ("name" := string)
                `andThen` \event -> succeed (EventCustom event)

        "Person" ->
            succeed Person
                |: ("name" := string)
                |: ("twitterName" := string)
                `andThen` \person -> succeed (PersonCustom person)

        "PlainStatement" ->
            succeed Plain
                |: ("languageCode" := string)
                |: ("name" := string)
                `andThen` \plain -> succeed (PlainCustom plain)

        "Tag" ->
            succeed Tag
                |: ("name" := string)
                |: ("statementId" := string)
                `andThen` \tag -> succeed (TagCustom tag)

        _ ->
            fail ("Unkown statement type: " ++ statementType)


decodeStatementsAutocompletionBody : Decoder StatementsAutocompletionBody
decodeStatementsAutocompletionBody =
    succeed StatementsAutocompletionBody
        |: ("data" := list decodeStatementAutocompletion)


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


filterPrefix : String -> Dict String value -> Dict String value
filterPrefix prefix dict =
    let
        prefixLength =
            String.length prefix
    in
        Dict.filter (\key value -> String.startsWith prefix key) dict
            |> Dict.toList
            |> List.map (\( key, value ) -> ( String.dropLeft prefixLength key, value ))
            |> Dict.fromList


initEventForm : EventForm
initEventForm =
    { name = ""
    }


initPersonForm : PersonForm
initPersonForm =
    { name = ""
    , twitterName = ""
    }


initPlainForm : PlainForm
initPlainForm =
    { languageCode = "en"
    , name = ""
    }


initStatementEmbed : StatementEmbed
initStatementEmbed =
    { autocomplete = ""
    , autocompleteMenuState = AutocompleteMenuHidden
    , autocompleter = Autocomplete.empty
    , autocompletions = []
    , editedMaybe = Nothing
    , selectedMaybe = Nothing
    }


initStatementForm : StatementForm
initStatementForm =
    { argumentType = ""
    , cited = initStatementEmbed
    , citedId = ""
    , claimId = ""
    , errors = Dict.empty
    , event = initStatementEmbed
    , eventId = ""
    , groundId = ""
    , kind = "PlainStatement"
    , languageCode = "en"
    , name = ""
    , person = initStatementEmbed
    , personId = ""
    , statementId = ""
    , twitterName = ""
    }
