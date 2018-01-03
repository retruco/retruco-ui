module Discussions.Item.State exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Constants exposing (discussionKeyIds)
import Data exposing (filterDataWithIds, idsUsedByIds, initData, mergeData)
import Dict
import Discussions.Item.Routes exposing (..)
import Discussions.Item.Types exposing (..)
import Http
import I18n
import Ideas.Index.State
import Interventions.Index.State
import Interventions.Index.Types
import Navigation
import Ports
import Questions.Index.State
import Requests
import Set
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { activeTab = NoTab
    , authentication = authentication
    , data = initData
    , discussionProperties = Nothing
    , embed = embed
    , httpError = Nothing
    , language = language
    , objectId = objectId
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        allData =
            mergeData data model.data

        usedIds =
            Set.singleton model.objectId
                |> (case model.discussionProperties of
                        Just discussionProperties ->
                            Set.union (Set.fromList <| List.map .id <| Array.toList discussionProperties)

                        Nothing ->
                            identity
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
        | activeTab =
            case model.activeTab of
                IdeasTab ideasModel ->
                    IdeasTab
                        (Ideas.Index.State.mergeModelData model.data ideasModel
                            |> Ideas.Index.State.propagateModelDataChange
                        )

                InterventionsTab interventionsModel ->
                    InterventionsTab
                        (Interventions.Index.State.mergeModelData model.data interventionsModel
                            |> Interventions.Index.State.propagateModelDataChange
                        )

                NoTab ->
                    model.activeTab

                QuestionsTab questionsModel ->
                    QuestionsTab
                        (Questions.Index.State.mergeModelData model.data questionsModel
                            |> Questions.Index.State.propagateModelDataChange
                        )

        -- TrashTab trashModel ->
        --     TrashTab
        --         (Trash.Index.State.mergeModelData model.data trashModel
        --             |> Trash.Index.State.propagateModelDataChange
        --         )
    }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | activeTab =
            case model.activeTab of
                IdeasTab ideasModel ->
                    IdeasTab <|
                        Ideas.Index.State.setContext authentication embed language ideasModel

                InterventionsTab interventionsModel ->
                    InterventionsTab <|
                        Interventions.Index.State.setContext authentication embed language interventionsModel

                NoTab ->
                    model.activeTab

                QuestionsTab questionsModel ->
                    QuestionsTab <|
                        Questions.Index.State.setContext authentication embed language questionsModel

        -- TrashTab trashModel ->
        --     TrashTab <|
        --         Trash.Index.State.setContext authentication embed language trashModel
        , authentication = authentication
        , embed = embed
        , language = language
    }


setDiscussionProperties : Maybe (Array Property) -> Model -> Model
setDiscussionProperties discussionPropertiesMaybe model =
    if discussionPropertiesMaybe == model.discussionProperties then
        model
    else
        { model
            | activeTab =
                let
                    discussionProperties =
                        Maybe.withDefault Array.empty discussionPropertiesMaybe
                in
                    case model.activeTab of
                        IdeasTab ideasModel ->
                            IdeasTab <|
                                Ideas.Index.State.setDiscussionProperties discussionProperties ideasModel

                        InterventionsTab interventionsModel ->
                            InterventionsTab <|
                                Interventions.Index.State.setDiscussionProperties discussionProperties interventionsModel

                        NoTab ->
                            model.activeTab

                        QuestionsTab questionsModel ->
                            QuestionsTab <|
                                Questions.Index.State.setDiscussionProperties discussionProperties questionsModel

            -- TrashTab trashModel ->
            --     TrashTab <|
            --         Trash.Index.State.setDiscussionProperties  discussionProperties  trashModel
            , discussionProperties = discussionPropertiesMaybe
        }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    List.filterMap identity
        [ case model.activeTab of
            IdeasTab ideasModel ->
                Just <|
                    Sub.map IdeasMsg
                        (Ideas.Index.State.subscriptions ideasModel)

            InterventionsTab interventionsModel ->
                Just <|
                    Sub.map InterventionsMsg
                        (Interventions.Index.State.subscriptions interventionsModel)

            NoTab ->
                Nothing

            QuestionsTab questionsModel ->
                Just <|
                    Sub.map QuestionsMsg
                        (Questions.Index.State.subscriptions questionsModel)

        -- TrashTab trashModel ->
        --     Just <|
        --         Sub.map TrashMsg
        --             (Trash.Index.State.subscriptions trashModel)
        ]
        |> Sub.batch


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IdeasMsg childMsg ->
            case model.activeTab of
                IdeasTab ideasModel ->
                    let
                        ( updatedIdeasModel, childCmd ) =
                            Ideas.Index.State.update childMsg ideasModel
                    in
                        ( { model | activeTab = IdeasTab updatedIdeasModel }
                        , Cmd.map translateIdeasMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        InterventionsMsg childMsg ->
            case model.activeTab of
                InterventionsTab interventionsModel ->
                    let
                        ( updatedInterventionsModel, childCmd ) =
                            Interventions.Index.State.update childMsg interventionsModel
                    in
                        ( { model | activeTab = InterventionsTab updatedInterventionsModel }
                        , Cmd.map translateInterventionsMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        ObjectUpserted dataWithId objectWrapper ->
            let
                ( activeTab, activeTabCmd ) =
                    case model.activeTab of
                        IdeasTab ideasModel ->
                            -- TODO
                            ( IdeasTab ideasModel, Cmd.none )

                        InterventionsTab interventionsModel ->
                            let
                                ( updatedInterventionsModel, childCmd ) =
                                    Interventions.Index.State.update
                                        (Interventions.Index.Types.ObjectUpserted dataWithId objectWrapper)
                                        interventionsModel
                            in
                                ( InterventionsTab updatedInterventionsModel
                                , Cmd.map translateInterventionsMsg childCmd
                                )

                        NoTab ->
                            ( NoTab, Cmd.none )

                        QuestionsTab questionsModel ->
                            -- TODO
                            ( QuestionsTab questionsModel, Cmd.none )

                discussionProperties =
                    case objectWrapper of
                        PropertyWrapper property ->
                            if
                                (property.objectId == model.objectId)
                                    && (List.member property.keyId discussionKeyIds)
                            then
                                case model.discussionProperties of
                                    Just discussionProperties ->
                                        if
                                            List.any
                                                (\discussionProperty -> discussionProperty.id == property.id)
                                                (Array.toList discussionProperties)
                                        then
                                            Just discussionProperties
                                        else
                                            Just <|
                                                Array.append
                                                    (Array.fromList [ property ])
                                                    discussionProperties

                                    Nothing ->
                                        Just <| Array.fromList [ property ]
                            else
                                model.discussionProperties

                        _ ->
                            model.discussionProperties
            in
                ( { model
                    | activeTab = activeTab
                    , discussionProperties = discussionProperties
                  }
                    |> mergeModelData dataWithId
                , activeTabCmd
                )

        QuestionsMsg childMsg ->
            case model.activeTab of
                QuestionsTab questionsModel ->
                    let
                        ( updatedQuestionsModel, childCmd ) =
                            Questions.Index.State.update childMsg questionsModel
                    in
                        ( { model | activeTab = QuestionsTab updatedQuestionsModel }
                        , Cmd.map translateQuestionsMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        Retrieve ->
            ( { model
                | httpError = Nothing
              }
                |> setDiscussionProperties Nothing
            , Requests.getProperties model.authentication model.showTrashed [ model.objectId ] discussionKeyIds []
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
                discussionProperties =
                    Just <|
                        Array.fromList <|
                            List.filterMap
                                (\id -> Dict.get id data.properties)
                                (Array.toList data.ids)

                language =
                    model.language
            in
                ( (setDiscussionProperties discussionProperties model)
                    |> mergeModelData data
                    |> propagateModelDataChange
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.PropertiesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Properties
                    }
                )


urlUpdate : Navigation.Location -> Route -> Model -> ( Model, Cmd Msg )
urlUpdate location route model =
    let
        authentication =
            model.authentication

        embed =
            model.embed

        language =
            model.language

        objectId =
            model.objectId

        ( unroutedModel, unroutedCmd ) =
            update Retrieve
                { model
                    | showTrashed = Urls.queryToggle "trashed" location
                }
    in
        case route of
            IdeasRoute ->
                let
                    ideasModel =
                        Ideas.Index.State.init authentication embed language objectId

                    ( updatedIdeasModel, updatedIdeasCmd ) =
                        Ideas.Index.State.urlUpdate location ideasModel
                in
                    { unroutedModel
                        | activeTab = IdeasTab updatedIdeasModel
                    }
                        ! [ unroutedCmd
                          , Cmd.map translateIdeasMsg updatedIdeasCmd
                          ]

            InterventionsRoute ->
                let
                    interventionsModel =
                        Interventions.Index.State.init authentication embed language objectId

                    ( updatedInterventionsModel, updatedInterventionsCmd ) =
                        Interventions.Index.State.urlUpdate location interventionsModel
                in
                    { unroutedModel
                        | activeTab = InterventionsTab updatedInterventionsModel
                    }
                        ! [ unroutedCmd
                          , Cmd.map translateInterventionsMsg updatedInterventionsCmd
                          ]

            QuestionsRoute ->
                let
                    questionsModel =
                        Questions.Index.State.init authentication embed language objectId

                    ( updatedQuestionsModel, updatedQuestionsCmd ) =
                        Questions.Index.State.urlUpdate location questionsModel
                in
                    { unroutedModel
                        | activeTab = QuestionsTab updatedQuestionsModel
                    }
                        ! [ unroutedCmd
                          , Cmd.map translateQuestionsMsg updatedQuestionsCmd
                          ]
