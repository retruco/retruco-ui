module Cards.Index.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.Index.Types exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (DataProxy, initData, mergeData)
import Urls


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
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
        Retrieve ->
            ( { model
                | httpError = Nothing
                , ids = Nothing
              }
            , let
                limit =
                    10

                offset =
                    0
              in
                -- TODO: Handle sort order.
                Requests.getCards
                    model.authentication
                    (Maybe.withDefault "" model.searchCriteria.term)
                    limit
                    offset
                    []
                    []
                    model.showTrashed
                    |> Http.send (ForSelf << Retrieved)
            )

        Retrieved (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Retrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel | ids = Just data.ids }, Cmd.none )

        SearchSortChanged searchSort ->
            update Submit { model | searchSort = searchSort }

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        Submit ->
            case convertControlsToSearchCriteria model of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                Ok searchCriteria ->
                    update Retrieve { model | errors = Dict.empty, searchCriteria = searchCriteria }


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
                    { description = I18n.translate language I18n.CardsDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Cards
                    }
              ]
