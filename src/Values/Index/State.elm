module Values.Index.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (DataProxy, initData, mergeData)
import Urls
import Values.Index.Types exposing (..)


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
    , count = 0
    , data = initData
    , errors = Dict.empty
    , httpError = Nothing
    , ids = Nothing
    , language = language
    , searchCriteria =
        { sort = "recent"
        , term = Nothing
        }
    , searchSort = "recent"
    , searchTerm = ""
    , showTrashed = False
    }


convertControlsToSearchCriteria : Model -> Result FormErrors SearchCriteria
convertControlsToSearchCriteria model =
    let
        errorsList =
            (List.filterMap
                (\( name, errorMaybe ) ->
                    case errorMaybe of
                        Just error ->
                            Just ( name, error )

                        Nothing ->
                            Nothing
                )
                [ ( "searchSort"
                  , if String.isEmpty model.searchSort then
                        Just "Missing sort criteria"
                    else
                        Nothing
                  )
                , ( "searchTerm"
                  , Nothing
                  )
                ]
            )
    in
        if List.isEmpty errorsList then
            Ok
                { sort = model.searchSort
                , term =
                    if String.isEmpty model.searchTerm then
                        Nothing
                    else
                        Just model.searchTerm
                }
        else
            Err (Dict.fromList errorsList)


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
        }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Retrieve offset ->
            ( { model | httpError = Nothing }
            , let
                limit =
                    40
              in
                Requests.getValues
                    model.authentication
                    model.searchCriteria.term
                    limit
                    offset
                    False
                    model.showTrashed
                    model.searchCriteria.sort
                    |> Http.send (ForSelf << Retrieved)
            )

        Retrieved (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Retrieved (Ok { count, data, offset }) ->
            let
                mergedModel =
                    mergeModelData data model

                existingIds =
                    Maybe.withDefault Array.empty model.ids

                ids =
                    if offset == Array.length existingIds then
                        Array.append existingIds data.ids
                    else
                        existingIds
            in
                ( { mergedModel
                    | count = count
                    , ids = Just ids
                  }
                , Cmd.none
                )

        SearchSortChanged searchSort ->
            update Submit { model | searchSort = searchSort }

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        Submit ->
            case (convertControlsToSearchCriteria model) of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                Ok searchCriteria ->
                    update (Retrieve 0)
                        { model
                            | errors = Dict.empty
                            , ids = Nothing
                            , searchCriteria = searchCriteria
                        }


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language

        ( newModel, cmd ) =
            update Submit
                { model
                    | searchTerm = Urls.querySearchTerm location
                    , showTrashed = Urls.queryToggle "trashed" location
                }
    in
        newModel
            ! [ cmd
              , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.ValuesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Values
                    }
              ]
