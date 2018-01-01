port module Ports exposing (..)

import Authenticator.Types exposing (Authentication)
import Configuration
import I18n
import Images
import Json.Encode
import Strings
import Types exposing (..)
import Urls


-- AUTHENTICATION


port storeAuthentication : Maybe User -> Cmd msg



-- DOCUMENT METADATA


type alias DocumentMetatags =
    { description : String
    , imageUrl : String
    , title : String
    , twitterName : String
    }


setDocumentMetadata : DocumentMetadata -> Cmd msg
setDocumentMetadata metadata =
    setDocumentMetatags
        { description = metadata.description
        , imageUrl = Urls.fullApiUrl metadata.imageUrl ++ "?dim=500"
        , title = metadata.title ++ " – " ++ Configuration.appTitle
        , twitterName = Configuration.twitterName
        }


setDocumentMetadataForStatementId : I18n.Language -> DataProxy a -> String -> Cmd msg
setDocumentMetadataForStatementId language data statementId =
    let
        statementText =
            Strings.idToString language data statementId
    in
        setDocumentMetadata
            { description = statementText
            , imageUrl = Images.idToImageUrl language data statementId
            , title = Strings.stringToHtmlTitle statementText
            }


port setDocumentMetatags : DocumentMetatags -> Cmd msg



-- GRAPHQL


type alias GraphqlInitArguments =
    { httpUrl : String
    , wsUrl : String
    }


type alias GraphqlObjectUpsertedArguments =
    { apiKey : String
    , need : List String
    }


type alias GraphqlPropertyUpsertedArguments =
    { keyIds : List String
    , objectIds : List String
    , valueIds : List String
    }


port graphqlInit : GraphqlInitArguments -> Cmd msg


port graphqlReset : String -> Cmd msg


port graphqlSubscribeToObjectUpserted : GraphqlObjectUpsertedArguments -> Cmd msg


port graphqlSubscribeToPropertyUpserted : GraphqlPropertyUpsertedArguments -> Cmd msg


port objectUpserted : (Json.Encode.Value -> msg) -> Sub msg


port propertyUpserted : (Json.Encode.Value -> msg) -> Sub msg


initGraphql : Cmd msg
initGraphql =
    graphqlInit
        { httpUrl = Configuration.apiUrl ++ "graphql"
        , wsUrl = "ws" ++ (String.dropLeft 4 Configuration.apiUrl) ++ "subscriptions"
        }


resetGraphql : Cmd msg
resetGraphql =
    graphqlReset "not used"


subscribeToObjectUpserted : Maybe Authentication -> List String -> Cmd msg
subscribeToObjectUpserted authentication need =
    graphqlSubscribeToObjectUpserted
        { apiKey =
            case authentication of
                Just authentication ->
                    authentication.apiKey

                Nothing ->
                    ""
        , need = need
        }


subscribeToPropertyUpserted : List String -> List String -> List String -> Cmd msg
subscribeToPropertyUpserted objectIds keyIds valueIds =
    graphqlSubscribeToPropertyUpserted
        { keyIds = keyIds
        , objectIds = objectIds
        , valueIds = valueIds
        }



-- IMAGE UPLOAD


type alias ImagePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (ImagePortData -> msg) -> Sub msg



-- SHARERS


port shareOnFacebook : String -> Cmd msg


port shareOnGooglePlus : String -> Cmd msg


port shareOnLinkedIn : String -> Cmd msg


port shareOnTwitter : String -> Cmd msg
