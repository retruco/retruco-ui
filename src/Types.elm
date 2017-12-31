module Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode


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
    , qualities : Dict String (List String)
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


type alias Data =
    DataProxy {}


type alias DataWithId =
    { ballots : Dict String Ballot
    , cards : Dict String Card
    , collections : Dict String Collection
    , id : String
    , properties : Dict String Property
    , users : Dict String User
    , values : Dict String TypedValue
    }


type alias DataWithIdBody =
    { data : DataWithId
    }


type alias DataWithIds =
    { ballots : Dict String Ballot
    , cards : Dict String Card
    , collections : Dict String Collection
    , ids : Array String
    , properties : Dict String Property
    , users : Dict String User
    , values : Dict String TypedValue
    }


type alias DataWithIdsBody =
    { count : Int
    , data : DataWithIds
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
    , qualities : Dict String (List String)
    , ratingCount : Int
    , ratingSum : Int
    , references : Dict String (List String)
    , subTypeIds : List String
    , tags : List (Dict String String)
    , trashed : Bool
    , type_ : String
    , valueId : String
    }


type alias QualityItem =
    { keyId : String
    , valueIds : List String
    }


type alias Statement a =
    { a
        | argumentCount : Int
        , -- TODO Use Maybe
          ballotId : String
        , createdAt : String
        , id : String
        , qualities : Dict String (List String)
        , ratingCount : Int
        , ratingSum : Int
        , trashed : Bool
        , type_ : String
    }


type StatementWrapper
    = CardWrapper Card
    | PropertyWrapper Property
    | TypedValueWrapper TypedValue


type alias TypedValue =
    { argumentCount : Int
    , -- TODO Use Maybe
      ballotId : String
    , createdAt : String
    , id : String
    , qualities : Dict String (List String)
    , ratingCount : Int
    , ratingSum : Int
    , schemaId : String
    , trashed : Bool
    , type_ : String
    , value : ValueWrapper
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


type ValueWrapper
    = BooleanWrapper Bool
    | EmailWrapper String
    | IdsArrayWrapper (List String)
    | ImagePathWrapper String
    | NumberWrapper Float
    | StringWrapper String
    | UrlWrapper String
    | WrongWrapper String String
