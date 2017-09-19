module Types exposing (..)

import Array exposing (Array)
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
    , symbol : Maybe String
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
    , ids : Array String
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
    , symbol : Maybe String
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
        , symbol : Maybe String
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
    , symbol : Maybe String
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
    , ids = Array.empty
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
                    let
                        hasNewBallot =
                            case Dict.get ballotId new.ballots of
                                Just newBallot ->
                                    not newBallot.deleted

                                Nothing ->
                                    False
                    in
                        if hasNewBallot then
                            Nothing
                        else
                            case Dict.get ballotId old.ballots of
                                Just oldBallot ->
                                    if oldBallot.deleted then
                                        Just ballotId
                                    else
                                        Nothing

                                Nothing ->
                                    Just ballotId
                )

        obsoleteBallotIds =
            Set.fromList <|
                List.concat
                    [ Dict.values new.cards |> filterObsoleteBallotIds
                    , Dict.values new.properties |> filterObsoleteBallotIds
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
