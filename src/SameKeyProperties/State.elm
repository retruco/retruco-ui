module SameKeyProperties.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Navigation
import Ports
import Requests
import SameKeyProperties.Types exposing (..)
import Types exposing (..)
import Urls
import Values.New.State


init : Maybe Authentication -> I18n.Language -> String -> String -> Model
init authentication language objectId keyId =
    { authentication = authentication
    , data = initData
    , httpError = Nothing
    , keyId = keyId
    , language = language
    , newValueModel = Values.New.State.init authentication language []
    , objectId = objectId
    , propertyIds = Nothing
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
    Sub.batch
        [ Sub.map NewValueMsg (Values.New.State.subscriptions model.newValueModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewValueMsg childMsg ->
            let
                ( updatedNewValueModel, childCmd ) =
                    Values.New.State.update childMsg model.newValueModel
            in
                ( { model | newValueModel = updatedNewValueModel }
                , Cmd.map translateNewValueMsg childCmd
                )

        RatingPosted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        RatingPosted (Ok body) ->
            ( { model
                | data = mergeData body.data model.data
              }
            , Cmd.none
            )

        Retrieve ->
            ( { model
                | httpError = Nothing
                , propertyIds = Nothing
              }
            , Requests.getObjectProperties model.authentication model.objectId model.keyId
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
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | propertyIds = Just data.ids
                  }
                , -- TODO
                  Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.CardsDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Cards
                    }
                )

        UnvoteRating statementId ->
            ( model
            , Requests.unrateStatement model.authentication statementId
                |> Http.send (ForSelf << RatingPosted)
            )

        Upserted (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        Upserted (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | propertyIds =
                        case model.propertyIds of
                            Just propertyIds ->
                                if List.member data.id propertyIds then
                                    Just propertyIds
                                else
                                    Just (data.id :: propertyIds)

                            Nothing ->
                                Just [ data.id ]
                  }
                , Cmd.none
                )

        ValueUpserted data ->
            ( mergeModelData data model
            , Requests.postProperty model.authentication model.objectId model.keyId data.id (Just 1)
                |> Http.send (ForSelf << Upserted)
            )

        VoteRatingDown statementId ->
            ( model
            , Requests.rateStatement model.authentication statementId -1
                |> Http.send (ForSelf << RatingPosted)
            )

        VoteRatingUp statementId ->
            ( model
            , Requests.rateStatement model.authentication statementId 1
                |> Http.send (ForSelf << RatingPosted)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve model
