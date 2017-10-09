module Proposals.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Proposals.New.Types exposing (..)
import Requests
import Task
import Types exposing (DataProxy, initDataId, mergeData)
import Urls
import Values.New.State
import Values.New.Types


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
    , data = initDataId
    , httpError = Nothing
    , language = language
    , newValueModel = Values.New.State.init authentication language [ "TextField" ]
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newValueModel = Values.New.State.mergeModelData mergedData model.newValueModel
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newValueModel = Values.New.State.setContext authentication language model.newValueModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.map NewValueMsg (Values.New.State.subscriptions model.newValueModel)


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewValueMsg childMsg ->
            let
                ( newValueModel, childCmd ) =
                    model.newValueModel
                        |> Values.New.State.setContext model.authentication model.language
                        |> Values.New.State.update childMsg
            in
                ( { model | newValueModel = newValueModel }
                , Cmd.map translateNewValueMsg childCmd
                )

        Rated (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Rated (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model

                ballot =
                    Dict.get body.data.id mergedModel.data.ballots
            in
                case ballot of
                    Just ballot ->
                        let
                            mergedData =
                                mergedModel.data

                            data =
                                { mergedData | id = ballot.statementId }
                        in
                            ( { mergedModel | data = data }
                            , Task.perform (\_ -> ForParent <| ProposalUpserted data) (Task.succeed ())
                            )

                    Nothing ->
                        ( mergedModel, Cmd.none )

        Submit ->
            update (NewValueMsg Values.New.Types.Submit) model

        Upserted data ->
            ( { model | data = mergeData data model.data }
            , Requests.rateStatement model.authentication data.id 1
                |> Http.send (ForSelf << Rated)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewProposalDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewProposal
            }
        )
