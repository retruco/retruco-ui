module Cards.Index.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.Index.Types exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Urls
import WebData exposing (..)


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
    , errors = Dict.empty
    , language = language
    , searchCriteria =
        { sort = "latest"
        , term = Nothing
        }
    , searchSort = "latest"
    , searchTerm = ""
    , showTrashed = False
    , webData = NotAsked
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


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Found (Err httpError) ->
            let
                _ =
                    Debug.log "Cards.State update Loaded Err" httpError

                newModel =
                    { model | webData = Failure httpError }
            in
                ( newModel, Cmd.none )

        Found (Ok body) ->
            ( { model | webData = Data (Loaded body) }
            , Cmd.none
            )

        Search ->
            let
                newModel =
                    { model | webData = Data (Loading (getData model.webData)) }

                cmd =
                    let
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
                            |> Http.send (ForSelf << Found)
            in
                ( newModel, cmd )

        SearchSortChanged searchSort ->
            ( { model | searchSort = searchSort }, Cmd.none )

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        Submit ->
            case (convertControlsToSearchCriteria model) of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                Ok searchCriteria ->
                    update
                        Search
                        { model | errors = Dict.empty, searchCriteria = searchCriteria }


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
