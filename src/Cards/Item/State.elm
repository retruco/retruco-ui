module Cards.Item.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Cards.Item.Routes exposing (..)
import Cards.Item.Types exposing (..)
import Constants exposing (duplicateOfKeyId)
import DebateProperties.SameObject.State
import Dict
import Discussions.Item.State
import Http
import I18n
import Navigation
import Ports
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
    , card = Nothing
    , data = initData
    , duplicatedByPropertyIds = Nothing
    , duplicateOfPropertyIds = Nothing
    , embed = embed
    , httpError = Nothing
    , id = id
    , language = language
    , sameKeyPropertiesModel = Nothing
    , showTrashed = False
    , toolbarModel = Nothing
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data

        card =
            Dict.get model.id mergedData.cards
    in
        { model
            | activeTab =
                case model.activeTab of
                    DebatePropertiesTab debatePropertiesModel ->
                        DebatePropertiesTab <|
                            DebateProperties.SameObject.State.mergeModelData mergedData debatePropertiesModel

                    DiscussionTab discussionModel ->
                        DiscussionTab <|
                            Discussions.Item.State.mergeModelData mergedData discussionModel

                    PropertiesAsValueTab propertiesAsValueModel ->
                        PropertiesAsValueTab <|
                            Properties.SameValue.State.mergeModelData mergedData propertiesAsValueModel

                    PropertiesTab propertiesModel ->
                        PropertiesTab <| Properties.SameObject.State.mergeModelData mergedData propertiesModel

                    _ ->
                        model.activeTab
            , card = card
            , data = mergedData
            , sameKeyPropertiesModel =
                case model.sameKeyPropertiesModel of
                    Just sameKeyPropertiesModel ->
                        Just <|
                            Properties.SameObjectAndKey.State.mergeModelData
                                mergedData
                                sameKeyPropertiesModel

                    Nothing ->
                        Nothing
            , toolbarModel =
                case card of
                    Just card ->
                        case model.toolbarModel of
                            Just toolbarModel ->
                                Just <| Statements.Toolbar.State.setModelData mergedData card toolbarModel

                            Nothing ->
                                Just <|
                                    Statements.Toolbar.State.init
                                        model.authentication
                                        model.embed
                                        model.language
                                        mergedData
                                        card

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

                DiscussionTab discussionModel ->
                    DiscussionTab <|
                        Discussions.Item.State.setContext authentication embed language discussionModel

                PropertiesAsValueTab propertiesAsValueModel ->
                    PropertiesAsValueTab <|
                        Properties.SameValue.State.setContext authentication embed language propertiesAsValueModel

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

            DiscussionTab discussionModel ->
                Just <| Sub.map DiscussionMsg (Discussions.Item.State.subscriptions discussionModel)

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
        CardRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        CardRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model

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
                        ++ [ Requests.getProperties
                                updatedModel.authentication
                                updatedModel.showTrashed
                                []
                                [ duplicateOfKeyId ]
                                [ updatedModel.id ]
                                |> Http.send (ForSelf << DuplicatedByRetrieved)
                           ]
                        ++ [ Requests.getProperties
                                updatedModel.authentication
                                updatedModel.showTrashed
                                [ updatedModel.id ]
                                [ duplicateOfKeyId ]
                                []
                                |> Http.send (ForSelf << DuplicateOfRetrieved)
                           ]
                      )

        DataUpdated data ->
            ( mergeModelData data model, Cmd.none )

        DuplicatedByRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        DuplicatedByRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel
                    | duplicatedByPropertyIds = Just <| Array.toList data.ids
                  }
                , Cmd.none
                )

        DuplicateOfRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        DuplicateOfRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel
                    | duplicateOfPropertyIds = Just <| Array.toList data.ids
                  }
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

        DiscussionMsg childMsg ->
            case model.activeTab of
                DiscussionTab discussionModel ->
                    let
                        ( updatedDiscussionModel, childCmd ) =
                            Discussions.Item.State.update childMsg discussionModel
                    in
                        ( { model | activeTab = DiscussionTab updatedDiscussionModel }
                        , Cmd.map translateDiscussionMsg childCmd
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
                | card = Nothing
                , duplicatedByPropertyIds = Nothing
                , duplicateOfPropertyIds = Nothing
                , httpError = Nothing
                , toolbarModel = Nothing
              }
            , Requests.getCard model.authentication model.id
                |> Http.send (ForSelf << CardRetrieved)
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

            DiscussionRoute discussionRoute ->
                let
                    discussionModel =
                        Discussions.Item.State.init authentication embed language id

                    ( updatedDiscussionModel, updatedDiscussionCmd ) =
                        Discussions.Item.State.urlUpdate location discussionRoute discussionModel
                in
                    { updatedModel
                        | activeTab = DiscussionTab updatedDiscussionModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translateDiscussionMsg updatedDiscussionCmd
                          ]

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
