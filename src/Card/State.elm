module Card.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Card.Types exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Urls
import WebData exposing (..)


init : Model
init =
    { authentication = Nothing
    , id = ""
    , language = I18n.English
    , webData = NotAsked
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Retrieve ->
            let
                newModel =
                    { model | webData = Data (Loading (getData model.webData)) }

                cmd =
                    let
                        limit =
                            Just 10
                    in
                        Requests.getCard
                            model.authentication
                            model.id
                            |> Http.send (ForSelf << Retrieved)
            in
                ( newModel, cmd )

        Retrieved (Err err) ->
            let
                _ =
                    Debug.log "Card.State update Loaded Err" err

                newModel =
                    { model | webData = Failure err }
            in
                ( newModel, Cmd.none )

        Retrieved (Ok body) ->
            let
                data =
                    body.data

                card =
                    Dict.get data.id data.cards
            in
                ( { model | webData = Data (Loaded body) }
                , case card of
                    Just card ->
                        -- TODO
                        Ports.setDocumentMetadata
                            { description = I18n.translate model.language I18n.CardsDescription
                            , imageUrl = Urls.appLogoFullUrl
                            , title = I18n.translate model.language I18n.Cards
                            }

                    Nothing ->
                        Cmd.none
                )


urlUpdate : Maybe Authentication -> I18n.Language -> Navigation.Location -> String -> Model -> ( Model, Cmd Msg )
urlUpdate authentication language location id model =
    update Retrieve
        { model
            | authentication = authentication
            , id = id
            , language = language
        }
