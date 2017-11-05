module Situations.NewSuggestion.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Navigation
import Ports
import Proposals.New.State
import Requests
import Situations.NewSuggestion.Types exposing (..)
import Task
import Types exposing (DataProxy, initDataId, mergeData)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language objectId =
    { authentication = authentication
    , data = initDataId
    , httpError = Nothing
    , language = language
    , newProposalModel = Proposals.New.State.init authentication language
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


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newProposalModel = Proposals.New.State.setContext authentication language model.newProposalModel
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
                        |> Proposals.New.State.setContext model.authentication model.language
                        |> Proposals.New.State.update childMsg
            in
                ( { model | newProposalModel = newProposalModel }
                , Cmd.map translateNewProposalMsg childCmd
                )

        ProposalUpserted data ->
            ( { model | data = mergeData data model.data }
            , Requests.postProperty model.authentication model.objectId "suggestion" data.id (Just 1)
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
                , Task.perform (\_ -> ForParent <| SuggestionUpserted data) (Task.succeed ())
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewSuggestionDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewSuggestion
            }
        )
