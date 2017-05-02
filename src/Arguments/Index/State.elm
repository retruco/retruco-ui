module Arguments.Index.State exposing (..)

import Arguments.Index.Types exposing (..)
import Arguments.New.State
import Authenticator.Types exposing (Authentication)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language objectId =
    { authentication = authentication
    , data = initData
    , httpError = Nothing
    , language = language
    , newArgumentModel = Arguments.New.State.init authentication language objectId []
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
            , newArgumentModel = Arguments.New.State.mergeModelData mergedData model.newArgumentModel
        }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewArgumentMsg (Arguments.New.State.subscriptions model.newArgumentModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewArgumentMsg childMsg ->
            let
                ( updatedNewArgumentModel, childCmd ) =
                    Arguments.New.State.update childMsg model.newArgumentModel
            in
                ( { model | newArgumentModel = updatedNewArgumentModel }
                , Cmd.map translateNewArgumentMsg childCmd
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
            , Requests.getDebateProperties model.authentication model.objectId
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

        Upserted data ->
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

        VotePropertyDown propertyId ->
            ( model
            , Requests.rateProperty model.authentication propertyId -1
                |> Http.send (ForSelf << RatingPosted)
            )

        VotePropertyUp propertyId ->
            ( model
            , Requests.rateProperty model.authentication propertyId 1
                |> Http.send (ForSelf << RatingPosted)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve model