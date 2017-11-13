module Properties.SameValue.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Navigation
import Ports
import Properties.SameValue.Types exposing (..)
import Requests
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language valueId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , httpError = Nothing
    , language = language
    , propertyIds = Nothing
    , showTrashed = False
    , valueId = valueId
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
        }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Retrieve ->
            ( { model
                | propertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getProperties model.authentication model.showTrashed [] [] [ model.valueId ]
                |> Http.send (ForSelf << Retrieved)
            )

        Retrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        Retrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | propertyIds = Just data.ids
                  }
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.PropertiesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Properties
                    }
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve
        { model
            | showTrashed = Urls.queryToggle "trashed" location
        }
