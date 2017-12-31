module Interventions.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Constants exposing (discussionKeyIds)
import Data exposing (initDataWithId, mergeData)
import Dict
import Http
import I18n
import Interventions.New.Types exposing (..)
import Navigation
import Ports
import Proposals.New.State
import Proposals.New.Types
import Requests
import Task
import Types exposing (DataProxy)
import Urls
import Values.New.Types


convertControls : Model -> Model
convertControls model =
    let
        keyIdError =
            if List.member model.keyId discussionKeyIds then
                Nothing
            else if model.keyId == "" then
                Just I18n.MissingValue
            else
                Just I18n.UnknownValue
    in
        { model
            | errors =
                case keyIdError of
                    Just keyIdError ->
                        Dict.singleton "keyId" keyIdError

                    Nothing ->
                        Dict.empty
        }


init : Maybe Authentication -> Bool -> I18n.Language -> String -> String -> List String -> Model
init authentication embed language objectId keyId keyIds =
    { authentication = authentication
    , data = initDataWithId
    , embed = embed
    , errors = Dict.empty
    , httpError = Nothing
    , keyId = keyId
    , keyIds = keyIds
    , language = language
    , newProposalModel = Proposals.New.State.init authentication embed language
    , objectId = objectId
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newProposalModel = Proposals.New.State.mergeModelData mergedData model.newProposalModel
        }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
        , newProposalModel = Proposals.New.State.setContext authentication embed language model.newProposalModel
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.map NewProposalMsg (Proposals.New.State.subscriptions model.newProposalModel)


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InterventionSpecificPropertyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        InterventionSpecificPropertyUpserted (Ok body) ->
            ( mergeModelData body.data model
            , Cmd.none
            )

        KeyIdChanged keyId ->
            ( convertControls { model | keyId = keyId }, Cmd.none )

        NewProposalMsg childMsg ->
            let
                ( newProposalModel, childCmd ) =
                    model.newProposalModel
                        |> Proposals.New.State.setContext model.authentication model.embed model.language
                        |> Proposals.New.State.update childMsg
            in
                ( { model | newProposalModel = newProposalModel }
                , Cmd.map translateNewProposalMsg childCmd
                )

        ProposalUpserted data ->
            { model | data = mergeData data model.data }
                ! ((if model.keyId == "intervention" then
                        []
                    else
                        [ Requests.postProperty model.authentication model.objectId model.keyId data.id (Just 1)
                            |> Http.send (ForSelf << InterventionSpecificPropertyUpserted)
                        ]
                   )
                    ++ [ Requests.postProperty model.authentication model.objectId "intervention" data.id (Just 1)
                            |> Http.send (ForSelf << Upserted)
                       ]
                  )

        Submit ->
            let
                newModel =
                    convertControls model
            in
                if Dict.isEmpty newModel.errors then
                    update (NewProposalMsg (Proposals.New.Types.NewValueMsg Values.New.Types.Submit)) newModel
                else
                    ( newModel, Cmd.none )

        Upserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Upserted (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model

                mergedData =
                    mergedModel.data

                data =
                    { mergedData | id = body.data.id }
            in
                ( mergedModel
                , Task.perform (\_ -> ForParent <| InterventionUpserted data) (Task.succeed ())
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewInterventionDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewIntervention
            }
        )
