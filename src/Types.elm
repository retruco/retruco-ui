module Types exposing (..)

-- import Autocomplete

import Dict exposing (Dict)
import Json.Decode


-- type alias Abuse =
--     { statementId : String
--     }
-- type alias Argument =
--     { argumentType : ArgumentType
--     , claimId : String
--     , groundId : String
--     }
-- type ArgumentType
--     = Because
--     | But
--     | Comment
--     | Example
-- type AutocompleteMenuState
--     = AutocompleteMenuHidden
--     | AutocompleteMenuSleeping
--     | AutocompleteMenuLoading
--     | AutocompleteMenuVisible


type alias Ballot =
    { deleted : Bool
    , id : String
    , rating : Int
    , statementId : String
    , updatedAt : String
    , voterId : String
    }


type alias BijectiveCardReference =
    { targetId : String
    , reverseKeyId : String
    }


type alias Card =
    { createdAt : String
    , deleted : Bool
    , id : String
    , properties : Dict String String
    , rating : Int
    , ratingCount : Int
    , ratingSum : Int
    , references : Dict String (List String)
    , subTypeIds : List String
    , tagIds : List String
    , type_ : String
    , usageIds : List String
    }


type alias CardAutocompletion =
    { autocomplete : String
    , card : Card
    , distance : Float
    }


type alias CardsAutocompletionBody =
    { data : List CardAutocompletion
    }



-- type alias Citation =
--     { citedId : String
--     , eventId : String
--     , personId : String
--     }


type alias Collection =
    { authorId : String
    , cardIds : List String
    , description : String
    , id : String
    , logo : Maybe String
    , name : String
    }


type alias DataId =
    { ballots : Dict String Ballot
    , cards : Dict String Card
    , collections : Dict String Collection
    , id : String
    , properties : Dict String Property
    , users : Dict String User
    , values : Dict String TypedValue
    }


type alias DataIdBody =
    { data : DataId
    }


type alias DataIds =
    { ballots : Dict String Ballot
    , cards : Dict String Card
    , collections : Dict String Collection
    , ids : List String
    , properties : Dict String Property
    , users : Dict String User
    , values : Dict String TypedValue
    }


type alias DataIdsBody =
    { count : Int
    , data : DataIds
    , limit : Int
    , offset : Int
    }


type alias DataProxy a =
    { a
        | ballots : Dict String Ballot
        , cards : Dict String Card
        , collections : Dict String Collection
        , properties : Dict String Property
        , users : Dict String User
        , values : Dict String TypedValue
    }


type alias DocumentMetadata =
    { description : String
    , imageUrl : String
    , title : String
    }



-- type alias Event =
--     { name : String
--     }
-- type alias EventForm =
--     Event


type Field
    = BooleanField Bool
    | CardIdField String
    | ImageField String
    | InputEmailField String
    | InputNumberField Float
    | InputTextField String
    | InputUrlField String
    | LocalizedInputTextField String String
    | LocalizedTextareaField String String
    | TextareaField String


type alias Flags =
    { language : String
    , authentication : Json.Decode.Value
    }


type alias FormErrors =
    Dict String String



-- type alias ModelFragment a =
--     { a
--         | ballotById : Dict String Ballot
--         , statementById : Dict String Statement
--         , statementIds : List String
--     }
-- type alias Person =
--     { name : String
--     , twitterName : String
--     }
-- type alias PersonForm =
--     Person
-- type alias Plain =
--     { languageCode : String
--     , name : String
--     }
-- type alias PlainForm =
--     Plain


type alias PopularTag =
    { count : Float
    , tagId : String
    }


type alias PopularTagsData =
    { popularity : List PopularTag
    , values : Dict String TypedValue
    }


type alias Property =
    { ballotId :
        String
        -- TODO Use Maybe
    , createdAt : String
    , deleted : Bool
    , id : String
    , keyId : String
    , objectId : String
    , properties : Dict String String
    , rating : Int
    , ratingCount : Int
    , ratingSum : Int
    , references : Dict String (List String)
    , subTypeIds : List String
    , tags : List (Dict String String)
    , type_ : String
    , valueId : String
    }


type alias SearchCriteria =
    { kinds : List String
    , languageCodeMaybe : Maybe String
    , sort : String
    , termMaybe : Maybe String
    }



-- type alias Statement =
--     { ballotIdMaybe : Maybe String
--     , createdAt : String
--     , custom : StatementCustom
--     , deleted : Bool
--     , groundIds : List String
--     , id : String
--     , isAbuse : Bool
--     , ratingCount : Int
--     , ratingSum : Int
--     }
-- type alias StatementAutocompletion =
--     { autocomplete : String
--     , distance : Float
--     , statement : Statement
--     }
-- type StatementCustom
--     = AbuseCustom Abuse
--     | ArgumentCustom Argument
--     | CitationCustom Citation
--     | EventCustom Event
--     | PersonCustom Person
--     | PlainCustom Plain
--     | TagCustom Tag
-- type alias StatementEmbed =
--     { autocomplete : String
--     , autocompleteMenuState : AutocompleteMenuState
--     , autocompleter : Autocomplete.State
--     , autocompletions : List StatementAutocompletion
--     , editedMaybe : Maybe PersonForm
--     , selectedMaybe : Maybe StatementAutocompletion
--     }
-- type alias StatementForm =
--     { argumentType : String
--     , cited : StatementEmbed
--     , citedId : String
--     , claimId : String
--     , errors : FormErrors
--     , event : StatementEmbed
--     , eventId : String
--     , groundId : String
--     , kind : String
--     , languageCode : String
--     , name : String
--     , person : StatementEmbed
--     , personId : String
--     , statementId : String
--     , twitterName : String
--     }
-- type alias StatementsAutocompletionBody =
--     { data : List StatementAutocompletion
--     }
-- type alias Tag =
--     { name : String
--     , statementId : String
--     }


type alias TypedValue =
    { createdAt : String
    , id : String
    , schemaId : String
    , type_ : String
    , value : ValueType
    , widgetId : String
    }


type alias User =
    { activated : Bool
    , apiKey : String
    , email : String
    , id : String
    , isAdmin : Bool
    , name : String
    , urlName : String
    }


type alias UserBody =
    { data : User
    }


type ValueType
    = BijectiveCardReferenceValue BijectiveCardReference
    | BooleanValue Bool
    | CardIdArrayValue (List String)
    | CardIdValue String
    | EmailValue String
    | ImagePathValue String
    | LocalizedStringValue (Dict String String)
    | NumberValue Float
    | StringValue String
    | UrlValue String
    | ValueIdArrayValue (List String)
    | ValueIdValue String
    | WrongValue String String



-- convertArgumentTypeToString : ArgumentType -> String
-- convertArgumentTypeToString argumentType =
--     case argumentType of
--         Because ->
--             "because"
--         But ->
--             "but"
--         Comment ->
--             "comment"
--         Example ->
--             "example"
-- convertStatementCustomToKind : StatementCustom -> String
-- convertStatementCustomToKind statementCustom =
--     case statementCustom of
--         AbuseCustom abuse ->
--             "Abuse"
--         ArgumentCustom argument ->
--             "Argument"
--         CitationCustom plain ->
--             "Citation"
--         EventCustom plain ->
--             "Event"
--         PersonCustom plain ->
--             "Person"
--         PlainCustom plain ->
--             "PlainStatement"
--         TagCustom tag ->
--             "Tag"
-- convertStatementFormToCustom : StatementForm -> StatementCustom
-- convertStatementFormToCustom form =
--     case form.kind of
--         "Abuse" ->
--             AbuseCustom
--                 { statementId = form.statementId
--                 }
--         "Argument" ->
--             ArgumentCustom
--                 { argumentType =
--                     case form.argumentType of
--                         "because" ->
--                             Because
--                         "but" ->
--                             But
--                         "comment" ->
--                             Comment
--                         "example" ->
--                             Example
--                         _ ->
--                             Comment
--                 , claimId = form.claimId
--                 , groundId = form.groundId
--                 }
--         "Citation" ->
--             CitationCustom
--                 { citedId = form.citedId
--                 , eventId = form.eventId
--                 , personId = form.personId
--                 }
--         "Event" ->
--             EventCustom
--                 { name = form.name
--                 }
--         "Person" ->
--             PersonCustom
--                 { name = form.name
--                 , twitterName = form.twitterName
--                 }
--         "PlainStatement" ->
--             PlainCustom
--                 { languageCode = form.languageCode
--                 , name = form.name
--                 }
--         "Tag" ->
--             TagCustom
--                 { name = form.name
--                 , statementId = form.statementId
--                 }
--         _ ->
--             -- TODO: Return a Result instead of a dummy PlainCustom.
--             PlainCustom
--                 { languageCode = "en"
--                 , name = "Unknown kind: " ++ form.kind
--                 }
-- filterPrefix : String -> Dict String value -> Dict String value
-- filterPrefix prefix dict =
--     let
--         prefixLength =
--             String.length prefix
--     in
--         Dict.filter (\key value -> String.startsWith prefix key) dict
--             |> Dict.toList
--             |> List.map (\( key, value ) -> ( String.dropLeft prefixLength key, value ))
--             |> Dict.fromList


getCard : Dict String Card -> String -> Card
getCard cards id =
    case Dict.get id cards of
        Nothing ->
            Debug.crash "getCard: Should never happen"

        Just card ->
            card


getOrderedCards : DataIds -> List Card
getOrderedCards { cards, ids } =
    List.map (getCard cards) ids


getOrderedProperties : DataIds -> List Property
getOrderedProperties { properties, ids } =
    List.map (getProperty properties) ids


getProperty : Dict String Property -> String -> Property
getProperty properties id =
    case Dict.get id properties of
        Nothing ->
            Debug.crash ("getProperty: Should never happen id=" ++ id)

        Just property ->
            property


getValue : Dict String TypedValue -> String -> TypedValue
getValue values id =
    case Dict.get id values of
        Nothing ->
            Debug.crash ("getValue: Should never happen id=" ++ id)

        Just value ->
            value



-- initEventForm : EventForm
-- initEventForm =
--     { name = ""
--     }
-- initPersonForm : PersonForm
-- initPersonForm =
--     { name = ""
--     , twitterName = ""
--     }
-- initPlainForm : PlainForm
-- initPlainForm =
--     { languageCode = "en"
--     , name = ""
--     }
-- initStatementEmbed : StatementEmbed
-- initStatementEmbed =
--     { autocomplete = ""
--     , autocompleteMenuState = AutocompleteMenuHidden
--     , autocompleter = Autocomplete.empty
--     , autocompletions = []
--     , editedMaybe = Nothing
--     , selectedMaybe = Nothing
--     }
-- initStatementForm : StatementForm
-- initStatementForm =
--     { argumentType = ""
--     , cited = initStatementEmbed
--     , citedId = ""
--     , claimId = ""
--     , errors = Dict.empty
--     , event = initStatementEmbed
--     , eventId = ""
--     , groundId = ""
--     , kind = "PlainStatement"
--     , languageCode = "en"
--     , name = ""
--     , person = initStatementEmbed
--     , personId = ""
--     , statementId = ""
--     , twitterName = ""
--     }


nameKeys : List String
nameKeys =
    [ "name" ]
