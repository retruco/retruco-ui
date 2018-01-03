module Interventions.Index.State exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Constants exposing (discussionKeyIds)
import Data exposing (filterDataWithIds, idsUsedByIds, initData, mergeData)
import Dict
import I18n
import Interventions.Index.Types exposing (..)
import Interventions.New.State
import Interventions.New.Types
import Navigation
import Set
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , ideaPropertyByValueId = Dict.empty
    , interventionProperties = Array.empty
    , language = language
    , newInterventionModel = Interventions.New.State.init authentication embed language objectId "intervention" discussionKeyIds
    , objectId = objectId
    , questionPropertyByValueId = Dict.empty
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        allData =
            mergeData data model.data

        usedIds =
            Set.singleton model.objectId
                |> Set.union (Set.fromList <| List.map .id <| Array.toList model.interventionProperties)
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
        | newInterventionModel =
            Interventions.New.State.mergeModelData model.data model.newInterventionModel
                |> Interventions.New.State.propagateModelDataChange
    }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
        , newInterventionModel =
            Interventions.New.State.setContext
                authentication
                embed
                language
                model.newInterventionModel
    }


setDiscussionProperties : Array Property -> Model -> Model
setDiscussionProperties discussionProperties model =
    { model
        | ideaPropertyByValueId =
            Dict.fromList <|
                List.map (\ideaProperty -> ( ideaProperty.valueId, ideaProperty )) <|
                    Array.toList <|
                        Array.filter
                            (\discussionProperty -> discussionProperty.keyId == "idea")
                            discussionProperties
        , interventionProperties =
            Array.filter
                (\discussionProperty -> discussionProperty.keyId == "intervention")
                discussionProperties
        , questionPropertyByValueId =
            Dict.fromList <|
                List.map (\questionProperty -> ( questionProperty.valueId, questionProperty )) <|
                    Array.toList <|
                        Array.filter
                            (\discussionProperty -> discussionProperty.keyId == "question")
                            discussionProperties
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.map NewInterventionMsg (Interventions.New.State.subscriptions model.newInterventionModel)


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InterventionUpserted data ->
            ( model, Cmd.none )

        NewInterventionMsg childMsg ->
            let
                ( updatedNewInterventionModel, childCmd ) =
                    Interventions.New.State.update childMsg model.newInterventionModel
            in
                ( { model | newInterventionModel = updatedNewInterventionModel }
                , Cmd.map translateNewInterventionMsg childCmd
                )

        ObjectUpserted dataWithId objectWrapper ->
            let
                ideaPropertyByValueId =
                    case objectWrapper of
                        PropertyWrapper property ->
                            if (property.objectId == model.objectId) && (property.keyId == "idea") then
                                Dict.insert property.id property model.ideaPropertyByValueId
                            else
                                model.ideaPropertyByValueId

                        _ ->
                            model.ideaPropertyByValueId

                interventionProperties =
                    case objectWrapper of
                        PropertyWrapper property ->
                            if (property.objectId == model.objectId) && (property.keyId == "intervention") then
                                if
                                    List.any
                                        (\interventionProperty -> interventionProperty.id == property.id)
                                        (Array.toList model.interventionProperties)
                                then
                                    model.interventionProperties
                                else
                                    Array.append (Array.fromList [ property ]) model.interventionProperties
                            else
                                model.interventionProperties

                        _ ->
                            model.interventionProperties

                ( newInterventionModel, newInterventionCmd ) =
                    let
                        ( updatedNewInterventionModel, childCmd ) =
                            Interventions.New.State.update
                                (Interventions.New.Types.ObjectUpserted dataWithId objectWrapper)
                                model.newInterventionModel
                    in
                        ( updatedNewInterventionModel
                        , Cmd.map translateNewInterventionMsg childCmd
                        )

                questionPropertyByValueId =
                    case objectWrapper of
                        PropertyWrapper property ->
                            if (property.objectId == model.objectId) && (property.keyId == "question") then
                                Dict.insert property.id property model.questionPropertyByValueId
                            else
                                model.questionPropertyByValueId

                        _ ->
                            model.questionPropertyByValueId
            in
                ( { model
                    | ideaPropertyByValueId = ideaPropertyByValueId
                    , interventionProperties = interventionProperties
                    , newInterventionModel = newInterventionModel
                    , questionPropertyByValueId = questionPropertyByValueId
                  }
                    |> mergeModelData dataWithId
                , newInterventionCmd
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    ( { model
        | showTrashed = Urls.queryToggle "trashed" location
      }
    , Cmd.none
    )
