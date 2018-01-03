module Properties.Item.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import Data exposing (initData, mergeData)
import DebateProperties.SameObject.State
import Dict
import Http
import I18n
import Navigation
import Ports
import Properties.Item.Routes exposing (..)
import Properties.Item.Types exposing (..)
import Properties.SameObject.State
import Properties.SameObjectAndKey.State
import Properties.SameValue.State
import Requests
import Statements.Toolbar.State
import Statements.Toolbar.Types
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language id =
    { activeTab = NoTab
    , authentication = authentication
    , data = initData
    , embed = embed
    , httpError = Nothing
    , id = id
    , language = language
    , property = Nothing
    , sameKeyPropertiesModel = Nothing
    , showTrashed = False
    , similarDebatePropertyIds = Nothing
    , toolbarModel = Nothing
    }


initIdeaProperty : Model -> Property -> IdeaPropertyModel
initIdeaProperty model property =
    { trashedDrawerIsOpen = False
    , trashedModel = Nothing
    }


initInterventionProperty : Model -> Property -> InterventionPropertyModel
initInterventionProperty model property =
    { trashedDrawerIsOpen = False
    , trashedModel = Nothing
    }


initQuestionProperty : Model -> Property -> QuestionPropertyModel
initQuestionProperty model property =
    { trashedDrawerIsOpen = False
    , trashedModel = Nothing
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , property = Dict.get model.id mergedData.properties
        }


propagateModelDataChange : Model -> Model
propagateModelDataChange model =
    { model
        | activeTab =
            case model.activeTab of
                MainTab mainModel ->
                    MainTab <|
                        case model.property of
                            Just property ->
                                case property.keyId of
                                    "idea" ->
                                        IdeaTabModel <| initIdeaProperty model property

                                    "intervention" ->
                                        InterventionTabModel <| initInterventionProperty model property

                                    "question" ->
                                        QuestionTabModel <| initQuestionProperty model property

                                    _ ->
                                        EmptyTabModel

                            Nothing ->
                                EmptyTabModel

                DebatePropertiesTab debatePropertiesModel ->
                    DebatePropertiesTab
                        (DebateProperties.SameObject.State.mergeModelData model.data debatePropertiesModel
                            |> DebateProperties.SameObject.State.propagateModelDataChange
                        )

                PropertiesAsValueTab propertiesAsValueModel ->
                    PropertiesAsValueTab
                        (Properties.SameValue.State.mergeModelData model.data propertiesAsValueModel
                            |> Properties.SameValue.State.propagateModelDataChange
                        )

                PropertiesTab propertiesModel ->
                    PropertiesTab
                        (Properties.SameObject.State.mergeModelData model.data propertiesModel
                            |> Properties.SameObject.State.propagateModelDataChange
                        )

                _ ->
                    model.activeTab
        , sameKeyPropertiesModel =
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    Just
                        (Properties.SameObjectAndKey.State.mergeModelData model.data sameKeyPropertiesModel
                            |> Properties.SameObjectAndKey.State.propagateModelDataChange
                        )

                Nothing ->
                    Nothing
        , toolbarModel =
            case model.property of
                Just property ->
                    case model.toolbarModel of
                        Just toolbarModel ->
                            Just
                                (Statements.Toolbar.State.setModelData model.data property toolbarModel
                                    |> Statements.Toolbar.State.propagateModelDataChange
                                )

                        Nothing ->
                            Just <|
                                Statements.Toolbar.State.init
                                    model.authentication
                                    model.embed
                                    model.language
                                    model.data
                                    property

                Nothing ->
                    Nothing
    }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | activeTab =
            case model.activeTab of
                DebatePropertiesTab debatePropertiesModel ->
                    DebatePropertiesTab <|
                        DebateProperties.SameObject.State.setContext authentication embed language debatePropertiesModel

                PropertiesAsValueTab propertiesAsValueModel ->
                    PropertiesAsValueTab <|
                        Properties.SameValue.State.setContext
                            authentication
                            embed
                            language
                            propertiesAsValueModel

                PropertiesTab propertiesModel ->
                    PropertiesTab <|
                        Properties.SameObject.State.setContext
                            authentication
                            embed
                            language
                            propertiesModel

                _ ->
                    model.activeTab
        , authentication = authentication
        , embed = embed
        , language = language
        , sameKeyPropertiesModel =
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    Just <|
                        Properties.SameObjectAndKey.State.setContext
                            authentication
                            embed
                            language
                            sameKeyPropertiesModel

                Nothing ->
                    Nothing
        , toolbarModel =
            case model.toolbarModel of
                Just toolbarModel ->
                    Just <| Statements.Toolbar.State.setContext authentication embed language toolbarModel

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
            ( mergeModelData data model
                |> propagateModelDataChange
            , Cmd.none
            )

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

        PropertiesAsValueMsg childMsg ->
            case model.activeTab of
                PropertiesAsValueTab propertiesAsValueModel ->
                    let
                        ( updatedPropertiesAsValueModel, childCmd ) =
                            Properties.SameValue.State.update childMsg propertiesAsValueModel
                    in
                        ( { model | activeTab = PropertiesAsValueTab updatedPropertiesAsValueModel }
                        , Cmd.map translatePropertiesAsValueMsg childCmd
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
                        |> propagateModelDataChange
            in
                ( { mergedModel
                    | similarDebatePropertyIds = Just <| Array.toList data.ids
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
                        |> propagateModelDataChange

                ( updatedModel, updatedCmd ) =
                    update (ToolbarMsg (Statements.Toolbar.Types.Start)) mergedModel
            in
                updatedModel
                    ! ([ updatedCmd
                       , Ports.setDocumentMetadataForStatementId
                            updatedModel.language
                            updatedModel.data
                            updatedModel.id
                       ]
                        ++ case updatedModel.property of
                            Just property ->
                                if List.member property.keyId debateKeyIds then
                                    [ Requests.getProperties
                                        updatedModel.authentication
                                        updatedModel.showTrashed
                                        [ property.objectId ]
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

        embed =
            model.embed

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
                        DebateProperties.SameObject.State.init authentication embed language id

                    ( updatedDebatePropertiesModel, updatedDebatePropertiesCmd ) =
                        DebateProperties.SameObject.State.urlUpdate location debatePropertiesModel
                in
                    { updatedModel
                        | activeTab = DebatePropertiesTab updatedDebatePropertiesModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translateDebatePropertiesMsg updatedDebatePropertiesCmd
                          ]

            MainRoute ->
                ( { updatedModel | activeTab = MainTab EmptyTabModel }
                , updatedCmd
                )

            PropertiesAsValueRoute ->
                let
                    propertiesAsValueModel =
                        Properties.SameValue.State.init authentication embed language id

                    ( updatedPropertiesAsValueModel, updatedPropertiesAsValueCmd ) =
                        Properties.SameValue.State.urlUpdate location propertiesAsValueModel
                in
                    { updatedModel
                        | activeTab = PropertiesAsValueTab updatedPropertiesAsValueModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translatePropertiesAsValueMsg updatedPropertiesAsValueCmd
                          ]

            PropertiesRoute ->
                let
                    propertiesModel =
                        Properties.SameObject.State.init authentication embed language id

                    ( updatedPropertiesModel, updatedPropertiesCmd ) =
                        Properties.SameObject.State.urlUpdate location propertiesModel
                in
                    { updatedModel
                        | activeTab = PropertiesTab updatedPropertiesModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translatePropertiesMsg updatedPropertiesCmd
                          ]

            SameObjectAndKeyPropertiesRoute keyId ->
                let
                    sameKeyPropertiesModel =
                        Properties.SameObjectAndKey.State.init authentication embed language id keyId

                    ( updatedSameObjectAndKeyPropertiesModel, updatedSameObjectAndKeyPropertiesCmd ) =
                        Properties.SameObjectAndKey.State.urlUpdate location sameKeyPropertiesModel
                in
                    { updatedModel | sameKeyPropertiesModel = Just updatedSameObjectAndKeyPropertiesModel }
                        ! [ updatedCmd
                          , Cmd.map translateSameKeyPropertiesMsg updatedSameObjectAndKeyPropertiesCmd
                          ]
