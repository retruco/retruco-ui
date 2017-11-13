module Ideas.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Ideas.New.Types exposing (..)
import Navigation
import Ports
import Proposals.New.State
import Requests
import Task
import Types exposing (DataProxy, initDataId, mergeData)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { authentication = authentication
    , data = initDataId
    , embed = embed
    , httpError = Nothing
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
            ( { model | data = mergeData data model.data }
            , Requests.postProperty model.authentication model.objectId "idea" data.id (Just 1)
                |> Http.send (ForSelf << Upserted)
            )

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
                , Task.perform (\_ -> ForParent <| IdeaUpserted data) (Task.succeed ())
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewIdeaDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewIdea
            }
        )
