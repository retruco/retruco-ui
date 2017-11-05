module Situations.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.New.State
import Cards.New.Types
import Http
import I18n
import Navigation
import Ports
import Requests
import Situations.New.Types exposing (..)
import Task
import Types exposing (DataProxy, initDataId, mergeData)
import Urls


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
    , data = initDataId
    , httpError = Nothing
    , id = ""
    , language = language
    , newCardModel = Cards.New.State.init authentication language
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newCardModel = Cards.New.State.mergeModelData mergedData model.newCardModel
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newCardModel = Cards.New.State.setContext authentication language model.newCardModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.map NewCardMsg (Cards.New.State.subscriptions model.newCardModel)


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCardMsg childMsg ->
            let
                ( newCardModel, childCmd ) =
                    model.newCardModel
                        |> Cards.New.State.setContext model.authentication model.language
                        |> Cards.New.State.update childMsg
            in
                ( { model | newCardModel = newCardModel }
                , Cmd.map translateNewCardMsg childCmd
                )

        TypePropertyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        TypePropertyUpserted (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model

                mergedData =
                    mergedModel.data

                data =
                    { mergedData | id = model.id }
            in
                ( { mergedModel | data = data }
                , Task.perform (\_ -> ForParent <| SituationUpserted data) (Task.succeed ())
                )

        Upserted data ->
            ( { model
                | data = mergeData data model.data
                , id = data.id
              }
            , Requests.postProperty model.authentication data.id "type" "situation" (Just 1)
                |> Http.send (ForSelf << TypePropertyUpserted)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewSituationDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewSituation
            }
        )
