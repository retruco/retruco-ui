port module Ports exposing (..)

import Configuration
import I18n
import Images
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
