module Discussions.Item.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Discussions.Item.Routes exposing (..)
import Discussions.Item.Types exposing (..)
import I18n
import Ideas.Index.State
import Interventions.Index.State
import Navigation
import Questions.Index.State
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language objectId =
    { activeTab = NoTab
    , authentication = authentication
    , data = initData
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


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | activeTab =
            case model.activeTab of
                IdeasTab ideasModel ->
                    IdeasTab <|
                        Ideas.Index.State.setContext authentication language ideasModel

                InterventionsTab interventionsModel ->
                    InterventionsTab <|
                        Interventions.Index.State.setContext authentication language interventionsModel

                QuestionsTab questionsModel ->
                    QuestionsTab <|
                        Questions.Index.State.setContext authentication language questionsModel

                -- TrashTab trashModel ->
                --     TrashTab <|
                --         Trash.Index.State.setContext authentication language trashModel
                _ ->
                    model.activeTab
        , authentication = authentication
        , language = language
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


urlUpdate : Navigation.Location -> Route -> Model -> ( Model, Cmd Msg )
urlUpdate location route model =
    let
        authentication =
            model.authentication

        language =
            model.language

        objectId =
            model.objectId

        showTrashed =
            Urls.queryToggle "trashed" location

        unroutedModel =
            { model
                | showTrashed = showTrashed
            }
    in
        case route of
            IdeasRoute ->
                let
                    ideasModel =
                        Ideas.Index.State.init authentication language objectId

                    ( updatedIdeasModel, updatedIdeasCmd ) =
                        Ideas.Index.State.urlUpdate location ideasModel
                in
                    ( { unroutedModel
                        | activeTab = IdeasTab updatedIdeasModel
                      }
                    , Cmd.map translateIdeasMsg updatedIdeasCmd
                    )

            InterventionsRoute ->
                let
                    interventionsModel =
                        Interventions.Index.State.init authentication language objectId

                    ( updatedInterventionsModel, updatedInterventionsCmd ) =
                        Interventions.Index.State.urlUpdate location interventionsModel
                in
                    ( { unroutedModel
                        | activeTab = InterventionsTab updatedInterventionsModel
                      }
                    , Cmd.map translateInterventionsMsg updatedInterventionsCmd
                    )

            QuestionsRoute ->
                let
                    questionsModel =
                        Questions.Index.State.init authentication language objectId

                    ( updatedQuestionsModel, updatedQuestionsCmd ) =
                        Questions.Index.State.urlUpdate location questionsModel
                in
                    ( { unroutedModel
                        | activeTab = QuestionsTab updatedQuestionsModel
                      }
                    , Cmd.map translateQuestionsMsg updatedQuestionsCmd
                    )
