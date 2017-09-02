module Properties.Item.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import DebateProperties.SameObject.State
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Properties.Item.Routes exposing (..)
import Properties.Item.Types exposing (..)
import Properties.SameObject.State
import Properties.SameObjectAndKey.State
import Requests
import Statements.Toolbar.State
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { activeTab = NoTab
    , authentication = authentication
    , data = initData
    , httpError = Nothing
    , id = id
    , language = language
    , property = Nothing
    , sameKeyPropertiesModel = Nothing
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
            | activeTab =
                case model.activeTab of
                    DebatePropertiesTab debatePropertiesModel ->
                        DebatePropertiesTab <|
                            DebateProperties.SameObject.State.mergeModelData mergedData debatePropertiesModel

                    PropertiesTab propertiesModel ->
                        PropertiesTab <| Properties.SameObject.State.mergeModelData mergedData propertiesModel

                    _ ->
                        model.activeTab
            , data = mergedData
            , property = property
            , sameKeyPropertiesModel =
                case model.sameKeyPropertiesModel of
                    Just sameKeyPropertiesModel ->
                        Just <| Properties.SameObjectAndKey.State.mergeModelData mergedData sameKeyPropertiesModel

                    Nothing ->
                        Nothing
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
        | activeTab =
            case model.activeTab of
                DebatePropertiesTab debatePropertiesModel ->
                    DebatePropertiesTab <|
                        DebateProperties.SameObject.State.setContext authentication language debatePropertiesModel

                PropertiesTab propertiesModel ->
                    PropertiesTab <| Properties.SameObject.State.setContext authentication language propertiesModel

                _ ->
                    model.activeTab
        , authentication = authentication
        , language = language
        , sameKeyPropertiesModel =
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    Just <| Properties.SameObjectAndKey.State.setContext authentication language sameKeyPropertiesModel

                Nothing ->
                    Nothing
        , toolbarModel =
            case model.toolbarModel of
                Just toolbarModel ->
                    Just <| Statements.Toolbar.State.setContext authentication language toolbarModel

                Nothing ->
                    Nothing
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    List.filterMap identity
        [ case model.activeTab of
            DebatePropertiesTab debatePropertiesModel ->
                Just <|
                    Sub.map DebatePropertiesMsg
                        (DebateProperties.SameObject.State.subscriptions debatePropertiesModel)

            PropertiesTab propertiesModel ->
                Just <| Sub.map PropertiesMsg (Properties.SameObject.State.subscriptions propertiesModel)

            _ ->
                Nothing
        , case model.sameKeyPropertiesModel of
            Just sameKeyPropertiesModel ->
                Just <|
                    Sub.map
                        SameKeyPropertiesMsg
                        (Properties.SameObjectAndKey.State.subscriptions sameKeyPropertiesModel)

            Nothing ->
                Nothing
        ]
        |> Sub.batch


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataUpdated data ->
            ( mergeModelData data model, Cmd.none )

        DebatePropertiesMsg childMsg ->
            case model.activeTab of
                DebatePropertiesTab debatePropertiesModel ->
                    let
                        ( updatedDebatePropertiesModel, childCmd ) =
                            DebateProperties.SameObject.State.update childMsg debatePropertiesModel
                    in
                        ( { model | activeTab = DebatePropertiesTab updatedDebatePropertiesModel }
                        , Cmd.map translateDebatePropertiesMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        PropertiesMsg childMsg ->
            case model.activeTab of
                PropertiesTab propertiesModel ->
                    let
                        ( updatedPropertiesModel, childCmd ) =
                            Properties.SameObject.State.update childMsg propertiesModel
                    in
                        ( { model | activeTab = PropertiesTab updatedPropertiesModel }
                        , Cmd.map translatePropertiesMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        Retrieve ->
            ( { model
                | httpError = Nothing
                , property = Nothing
                , similarDebatePropertyIds = Nothing
                , toolbarModel = Nothing
              }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        SameKeyPropertiesMsg childMsg ->
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    let
                        ( updatedSameObjectAndKeyPropertiesModel, childCmd ) =
                            Properties.SameObjectAndKey.State.update childMsg sameKeyPropertiesModel
                    in
                        ( { model | sameKeyPropertiesModel = Just updatedSameObjectAndKeyPropertiesModel }
                        , Cmd.map translateSameKeyPropertiesMsg childCmd
                        )

                Nothing ->
                    ( model, Cmd.none )

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
                mergedModel =
                    mergeModelData data model
            in
                mergedModel
                    ! ([ Ports.setDocumentMetadataForStatementId mergedModel.language mergedModel.data mergedModel.id ]
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

        showTrashed =
            Urls.queryToggle "trashed" location

        unroutedModel =
            { model
                | sameKeyPropertiesModel = Nothing
                , showTrashed = showTrashed
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            DebatePropertiesRoute ->
                let
                    debatePropertiesModel =
                        DebateProperties.SameObject.State.init authentication language id

                    ( updatedDebatePropertiesModel, updatedArgumentsCmd ) =
                        DebateProperties.SameObject.State.urlUpdate location debatePropertiesModel
                in
                    { updatedModel
                        | activeTab = DebatePropertiesTab updatedDebatePropertiesModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translateDebatePropertiesMsg updatedArgumentsCmd
                          ]

            PropertiesRoute ->
                let
                    propertiesModel =
                        Properties.SameObject.State.init authentication language id

                    ( updatedPropertiesModel, updatedArgumentsCmd ) =
                        Properties.SameObject.State.urlUpdate location propertiesModel
                in
                    { updatedModel
                        | activeTab = PropertiesTab updatedPropertiesModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translatePropertiesMsg updatedArgumentsCmd
                          ]

            SameObjectAndKeyPropertiesRoute keyId ->
                let
                    sameKeyPropertiesModel =
                        Properties.SameObjectAndKey.State.init authentication language id keyId

                    ( updatedSameObjectAndKeyPropertiesModel, updatedSameObjectAndKeyPropertiesCmd ) =
                        Properties.SameObjectAndKey.State.urlUpdate location sameKeyPropertiesModel
                in
                    { updatedModel | sameKeyPropertiesModel = Just updatedSameObjectAndKeyPropertiesModel }
                        ! [ updatedCmd
                          , Cmd.map translateSameKeyPropertiesMsg updatedSameObjectAndKeyPropertiesCmd
                          ]
