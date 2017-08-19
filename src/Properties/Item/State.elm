module Properties.Item.State exposing (..)

import Arguments.New.State
import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Properties.Item.Routes exposing (..)
import Properties.Item.Types exposing (..)
import Requests
import Statements.Toolbar.State
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { authentication = authentication
    , data = initData
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , id = id
    , language = language
    , newArgumentModel = Arguments.New.State.init authentication language id []
    , property = Nothing
    , showTrashed = False
    , similarDebatePropertyIds = Nothing
    , toolbarModel = Nothing
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data

        property =
            Dict.get model.id mergedData.properties
    in
        { model
            | data = mergedData
            , newArgumentModel = Arguments.New.State.mergeModelData mergedData model.newArgumentModel
            , property = property
            , toolbarModel =
                case property of
                    Just property ->
                        case model.toolbarModel of
                            Just toolbarModel ->
                                Just <| Statements.Toolbar.State.setData mergedData property toolbarModel

                            Nothing ->
                                Just <|
                                    Statements.Toolbar.State.init
                                        model.authentication
                                        model.language
                                        mergedData
                                        property

                    Nothing ->
                        Nothing
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
        , newArgumentModel = Arguments.New.State.setContext authentication language model.newArgumentModel
        , toolbarModel =
            case model.toolbarModel of
                Just toolbarModel ->
                    Just <| Statements.Toolbar.State.setContext authentication language toolbarModel

                Nothing ->
                    Nothing
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map NewArgumentMsg (Arguments.New.State.subscriptions model.newArgumentModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataUpdated data ->
            ( mergeModelData data model, Cmd.none )

        DebatePropertiesRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        DebatePropertiesRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel
                    | debatePropertyIds = Just data.ids
                  }
                , Cmd.none
                )

        DebatePropertyUpserted data ->
            let
                mergedModel =
                    mergeModelData data model

                language =
                    model.language
            in
                ( { mergedModel
                    | debatePropertyIds =
                        case model.debatePropertyIds of
                            Just debatePropertyIds ->
                                if List.member data.id debatePropertyIds then
                                    Just debatePropertyIds
                                else
                                    Just (data.id :: debatePropertyIds)

                            Nothing ->
                                Just [ data.id ]
                  }
                , Cmd.none
                )

        NewArgumentMsg childMsg ->
            let
                ( updatedNewArgumentModel, childCmd ) =
                    Arguments.New.State.update childMsg model.newArgumentModel
            in
                ( { model | newArgumentModel = updatedNewArgumentModel }
                , Cmd.map translateNewArgumentMsg childCmd
                )

        Retrieve ->
            ( { model
                | debatePropertyIds = Nothing
                , httpError = Nothing
                , property = Nothing
                , similarDebatePropertyIds = Nothing
                , toolbarModel = Nothing
              }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        SimilarDebatePropertiesRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        SimilarDebatePropertiesRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel
                    | similarDebatePropertyIds = Just data.ids
                  }
                , Cmd.none
                )

        ToolbarMsg childMsg ->
            case model.toolbarModel of
                Just toolbarModel ->
                    let
                        ( updatedToolbarModel, childCmd ) =
                            Statements.Toolbar.State.update childMsg toolbarModel
                    in
                        ( { model | toolbarModel = Just updatedToolbarModel }
                        , Cmd.map translateToolbarMsg childCmd
                        )

                Nothing ->
                    ( model, Cmd.none )

        ValueRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        ValueRetrieved (Ok { data }) ->
            let
                language =
                    model.language

                mergedModel =
                    mergeModelData data model
            in
                mergedModel
                    ! ([ Ports.setDocumentMetadata
                            { description = I18n.translate language I18n.ValuesDescription
                            , imageUrl = Urls.appLogoFullUrl
                            , title = I18n.translate language I18n.Values
                            }
                       , Requests.getObjectProperties model.authentication model.showTrashed model.id debateKeyIds []
                            |> Http.send (ForSelf << DebatePropertiesRetrieved)
                       ]
                        ++ case mergedModel.property of
                            Just property ->
                                if List.member property.keyId debateKeyIds then
                                    [ Requests.getObjectProperties
                                        model.authentication
                                        model.showTrashed
                                        property.objectId
                                        (List.filter
                                            (\debateKeyId -> debateKeyId /= property.keyId)
                                            debateKeyIds
                                        )
                                        [ property.valueId ]
                                        |> Http.send (ForSelf << SimilarDebatePropertiesRetrieved)
                                    ]
                                else
                                    []

                            Nothing ->
                                []
                      )


urlUpdate : Navigation.Location -> Route -> Model -> ( Model, Cmd Msg )
urlUpdate location route model =
    let
        authentication =
            model.authentication

        id =
            model.id

        language =
            model.language

        unroutedModel =
            { model
              -- | argumentsModel = Nothing
              -- , sameKeyPropertiesModel = Nothing
                | showTrashed = Urls.queryToggle "trashed" location
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            IndexRoute ->
                ( updatedModel, updatedCmd )
