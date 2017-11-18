module Questions.Index.State exposing (..)

import Array exposing (Array)
import Authenticator.Types exposing (Authentication)
import I18n
import Questions.Index.Types exposing (..)
import Interventions.New.State
import Navigation
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , language = language
    , newInterventionModel = Interventions.New.State.init authentication embed language objectId "question" [ "question" ]
    , objectId = objectId
    , questionProperties = Array.empty
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newInterventionModel = Interventions.New.State.mergeModelData mergedData model.newInterventionModel
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
        | questionProperties =
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
                ( updatedNewQuestionModel, childCmd ) =
                    Interventions.New.State.update childMsg model.newInterventionModel
            in
                ( { model | newInterventionModel = updatedNewQuestionModel }
                , Cmd.map translateNewInterventionMsg childCmd
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    ( { model
        | showTrashed = Urls.queryToggle "trashed" location
      }
    , Cmd.none
    )
