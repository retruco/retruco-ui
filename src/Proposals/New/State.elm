module Proposals.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Data exposing (filterDataWithIds, idsUsedByIds, initDataWithId, mergeData)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Proposals.New.Types exposing (..)
import Requests
import Set
import Task
import Types exposing (DataProxy)
import Urls
import Values.New.State
import Values.New.Types


init : Maybe Authentication -> Bool -> I18n.Language -> Model
init authentication embed language =
    { authentication = authentication
    , data = initDataWithId
    , embed = embed
    , httpError = Nothing
    , language = language
    , newValueModel = Values.New.State.init authentication embed language [ "TextField" ]
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        allData =
            mergeData data model.data

        usedIds =
            (if String.isEmpty allData.id then
                Set.empty
             else
                Set.singleton allData.id
            )
                |> idsUsedByIds allData

        mergedData =
            filterDataWithIds allData usedIds
    in
        { model
            | data = mergedData
        }


propagateModelDataChange : Model -> Model
propagateModelDataChange model =
    { model
        | newValueModel =
            Values.New.State.mergeModelData model.data model.newValueModel
                |> Values.New.State.propagateModelDataChange
    }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
        , newValueModel = Values.New.State.setContext authentication embed language model.newValueModel
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
                        -- |> Values.New.State.setContext model.authentication model.embed model.language
                        |> Values.New.State.update childMsg
            in
                ( { model | newValueModel = newValueModel }
                , Cmd.map translateNewValueMsg childCmd
                )

        ObjectUpserted dataWithId objectWrapper ->
            let
                ( newValueModel, newValueCmd ) =
                    let
                        ( updatedNewValueModel, childCmd ) =
                            Values.New.State.update
                                (Values.New.Types.ObjectUpserted dataWithId objectWrapper)
                                model.newValueModel
                    in
                        ( updatedNewValueModel
                        , Cmd.map translateNewValueMsg childCmd
                        )
            in
                ( { model
                    | newValueModel = newValueModel
                  }
                    |> mergeModelData dataWithId
                , newValueCmd
                )

        Rated (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Rated (Ok body) ->
            let
                ballot =
                    Dict.get body.data.id body.data.ballots
            in
                case ballot of
                    Just ballot ->
                        let
                            data =
                                model.data

                            dataWithId =
                                { data | id = ballot.statementId }
                        in
                            ( { model | data = dataWithId }
                                |> mergeModelData body.data
                                |> propagateModelDataChange
                            , Task.perform (\_ -> ForParent <| ProposalUpserted dataWithId) (Task.succeed ())
                            )

                    Nothing ->
                        ( model
                            |> mergeModelData body.data
                            |> propagateModelDataChange
                        , Cmd.none
                        )

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
