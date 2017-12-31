module Discussions.Item.State exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import Constants exposing (discussionKeyIds)
import Data exposing (initData, mergeData)
import Decoders
import Dict
import Discussions.Item.Routes exposing (..)
import Discussions.Item.Types exposing (..)
import Http
import I18n
import Ideas.Index.State
import Interventions.Index.State
import Json.Decode
import Navigation
import Ports
import Questions.Index.State
import Requests
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
        mergedData =
            mergeData data model.data
    in
        { model
            | activeTab =
                case model.activeTab of
                    IdeasTab ideasModel ->
                        IdeasTab <|
                            Ideas.Index.State.mergeModelData mergedData ideasModel

                    InterventionsTab interventionsModel ->
                        InterventionsTab <|
                            Interventions.Index.State.mergeModelData mergedData interventionsModel

                    QuestionsTab questionsModel ->
                        QuestionsTab <|
                            Questions.Index.State.mergeModelData mergedData questionsModel

                    -- TrashTab trashModel ->
                    --     TrashTab <|
                    --         Trash.Index.State.mergeModelData mergedData trashModel
                    _ ->
                        model.activeTab
            , data = mergedData
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

                QuestionsTab questionsModel ->
                    QuestionsTab <|
                        Questions.Index.State.setContext authentication embed language questionsModel

                -- TrashTab trashModel ->
                --     TrashTab <|
                --         Trash.Index.State.setContext authentication embed language trashModel
                _ ->
                    model.activeTab
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

                        QuestionsTab questionsModel ->
                            QuestionsTab <|
                                Questions.Index.State.setDiscussionProperties discussionProperties questionsModel

                        -- TrashTab trashModel ->
                        --     TrashTab <|
                        --         Trash.Index.State.setDiscussionProperties  discussionProperties  trashModel
                        _ ->
                            model.activeTab
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

            QuestionsTab questionsModel ->
                Just <|
                    Sub.map QuestionsMsg
                        (Questions.Index.State.subscriptions questionsModel)

            -- TrashTab trashModel ->
            --     Just <|
            --         Sub.map TrashMsg
            --             (Trash.Index.State.subscriptions trashModel)
            _ ->
                Nothing
        , Just <| Ports.propertyUpserted PropertyUpserted
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

        PropertyUpserted propertyJson ->
            case Json.Decode.decodeValue Decoders.graphqlPropertyDecoder propertyJson of
                Err message ->
                    let
                        _ =
                            Debug.log "PropertyUpserted Decode error:" message

                        _ =
                            Debug.log "PropertyUpserted JSON:" propertyJson
                    in
                        ( model, Cmd.none )

                Ok data ->
                    let
                        mergedModel =
                            mergeModelData data model

                        propertyId =
                            data.id

                        discussionProperties =
                            case Dict.get propertyId mergedModel.data.properties of
                                Just property ->
                                    if
                                        (property.objectId == model.objectId)
                                            && (List.member property.keyId discussionKeyIds)
                                    then
                                        case mergedModel.discussionProperties of
                                            Just discussionProperties ->
                                                if
                                                    List.any
                                                        (\discussionProperty -> discussionProperty.id == propertyId)
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
                                        mergedModel.discussionProperties

                                Nothing ->
                                    mergedModel.discussionProperties
                    in
                        ( setDiscussionProperties discussionProperties mergedModel
                        , Cmd.none
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
                language =
                    model.language

                mergedModel =
                    mergeModelData data model

                properties =
                    mergedModel.data.properties

                discussionProperties =
                    Just <|
                        Array.fromList <|
                            List.filterMap
                                (\id -> Dict.get id properties)
                                (Array.toList data.ids)
            in
                (setDiscussionProperties discussionProperties mergedModel)
                    ! [ Ports.subscribeToPropertyUpserted [ model.objectId ] discussionKeyIds []
                      , Ports.setDocumentMetadata
                            { description = I18n.translate language I18n.PropertiesDescription
                            , imageUrl = Urls.appLogoFullUrl
                            , title = I18n.translate language I18n.Properties
                            }
                      ]


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
