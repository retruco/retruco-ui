module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Set


type alias Argument =
    { id : String
    , keyId : String
    , ratingCount : Int
    , ratingSum : Int
    , valueId : String
    }


type alias Ballot =
    { deleted : Bool
    , id : String
    , rating : Int
    , statementId : String
    , updatedAt : String
    , voterId : String
    }


type alias Card =
    { argumentCount : Int
    , ballotId : String
    , createdAt : String
    , id : String
    , properties : Dict String (List String)
    , ratingCount : Int
    , ratingSum : Int
    , references : Dict String (List String)
    , subTypeIds : List String
    , tagIds : List String
    , trashed : Bool
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


type Field
    = BooleanField Bool
    | CardIdField String
    | ImageField String
    | InputEmailField String
    | InputNumberField Float
    | InputTextField (Maybe String) String
    | InputUrlField String
    | TextareaField (Maybe String) String
    | ValueIdField String


type alias Flags =
    { language : String
    , authentication : Json.Decode.Value
    }


type alias FormErrors =
    Dict String String


type alias PopularTag =
    { count : Float
    , tagId : String
    }


type alias PopularTagsData =
    { popularity : List PopularTag
    , values : Dict String TypedValue
    }


type alias Property =
    { argumentCount : Int
    , -- TODO Use Maybe
      ballotId : String
    , createdAt : String
    , id : String
    , keyId : String
    , objectId : String
    , properties : Dict String (List String)
    , ratingCount : Int
    , ratingSum : Int
    , references : Dict String (List String)
    , subTypeIds : List String
    , tags : List (Dict String String)
    , trashed : Bool
    , type_ : String
    , valueId : String
    }


type alias Statement a =
    { a
        | argumentCount : Int
        , -- TODO Use Maybe
          ballotId : String
        , createdAt : String
        , id : String
        , properties : Dict String (List String)
        , ratingCount : Int
        , ratingSum : Int
        , trashed : Bool
        , type_ : String
    }


type alias TypedValue =
    { argumentCount : Int
    , -- TODO Use Maybe
      ballotId : String
    , createdAt : String
    , id : String
    , properties : Dict String (List String)
    , ratingCount : Int
    , ratingSum : Int
    , schemaId : String
    , trashed : Bool
    , type_ : String
    , value : ValueType
    , widgetId : String
    }


type alias TypedValueAutocompletion =
    { autocomplete : String
    , distance : Float
    , value : TypedValue
    }


type alias TypedValuesAutocompletionBody =
    { data : List TypedValueAutocompletion
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
    = BooleanValue Bool
    | EmailValue String
    | IdsArrayValue (List String)
    | ImagePathValue String
    | NumberValue Float
    | StringValue String
    | UrlValue String
    | WrongValue String String


cardSubTypeIdsIntersect : List String -> List String -> Bool
cardSubTypeIdsIntersect cardSubTypeIds1 cardSubTypeIds2 =
    List.any (\subTypeId -> List.member subTypeId cardSubTypeIds2)
        cardSubTypeIds1


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


initData : DataProxy {}
initData =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


initDataId : DataId
initDataId =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , id = ""
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


initDataIds : DataIds
initDataIds =
    { ballots = Dict.empty
    , cards = Dict.empty
    , collections = Dict.empty
    , ids = []
    , properties = Dict.empty
    , users = Dict.empty
    , values = Dict.empty
    }


mergeData : DataProxy a -> DataProxy b -> DataProxy b
mergeData new old =
    let
        -- Remove old ballots that are not present in new.
        filterObsoleteBallotIds =
            List.filterMap
                (\{ ballotId } ->
                    if (not <| Dict.member ballotId new.ballots) && Dict.member ballotId old.ballots then
                        Just ballotId
                    else
                        Nothing
                )

        obsoleteBallotIds =
            Set.fromList <|
                List.concat
                    [ Dict.values new.properties |> filterObsoleteBallotIds
                    , Dict.values new.values |> filterObsoleteBallotIds
                    ]

        oldBallots =
            Dict.filter
                (\ballotId _ -> not <| Set.member ballotId obsoleteBallotIds)
                old.ballots
    in
        { old
            | ballots = Dict.union new.ballots oldBallots
            , cards = Dict.union new.cards old.cards
            , collections = Dict.union new.collections old.collections
            , properties = Dict.union new.properties old.properties
            , users = Dict.union new.users old.users
            , values = Dict.union new.values old.values
        }


mergeDataId : DataId -> DataId -> DataId
mergeDataId new old =
    let
        mergedData =
            mergeData new old
    in
        { mergedData
            | id =
                if String.isEmpty new.id then
                    old.id
                else
                    new.id
        }


mergeDataIds : DataIds -> DataIds -> DataIds
mergeDataIds new old =
    let
        mergedData =
            mergeData new old
    in
        { mergedData
            | ids = List.append old.ids new.ids
        }
