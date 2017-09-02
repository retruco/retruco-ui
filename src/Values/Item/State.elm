module Values.Item.State exposing (..)

import Authenticator.Types exposing (Authentication)
import DebateProperties.Index.State
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Properties.SameObjectAndKey.State
import Requests
import Statements.Toolbar.State
import Types exposing (..)
import Urls
import Values.Item.Routes exposing (..)
import Values.Item.Types exposing (..)


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { activeTab = PropertiesTab
    , authentication = authentication
    , data = initData
    , httpError = Nothing
    , id = id
    , language = language
    , sameObjectAndKeyPropertiesModel = Nothing
    , showTrashed = False
    , toolbarModel = Nothing
    , typedValue = Nothing
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data

        typedValue =
            Dict.get model.id mergedData.values
    in
        { model
            | activeTab =
                case model.activeTab of
                    DebatePropertiesTab debatePropertiesModel ->
                        DebatePropertiesTab <| DebateProperties.Index.State.mergeModelData mergedData debatePropertiesModel

                    _ ->
                        model.activeTab
            , data = mergedData
            , sameObjectAndKeyPropertiesModel =
                case model.sameObjectAndKeyPropertiesModel of
                    Just sameObjectAndKeyPropertiesModel ->
                        Just <| Properties.SameObjectAndKey.State.mergeModelData mergedData sameObjectAndKeyPropertiesModel

                    Nothing ->
                        Nothing
            , toolbarModel =
                case typedValue of
                    Just typedValue ->
                        case model.toolbarModel of
                            Just toolbarModel ->
                                Just <| Statements.Toolbar.State.setData mergedData typedValue toolbarModel

                            Nothing ->
                                Just <|
                                    Statements.Toolbar.State.init
                                        model.authentication
                                        model.language
                                        mergedData
                                        typedValue

                    Nothing ->
                        Nothing
            , typedValue = typedValue
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | activeTab =
            case model.activeTab of
                DebatePropertiesTab debatePropertiesModel ->
                    DebatePropertiesTab <| DebateProperties.Index.State.setContext authentication language debatePropertiesModel

                _ ->
                    model.activeTab
        , authentication = authentication
        , language = language
        , sameObjectAndKeyPropertiesModel =
            case model.sameObjectAndKeyPropertiesModel of
                Just sameObjectAndKeyPropertiesModel ->
                    Just <| Properties.SameObjectAndKey.State.setContext authentication language sameObjectAndKeyPropertiesModel

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
                Just <| Sub.map DebatePropertiesMsg (DebateProperties.Index.State.subscriptions debatePropertiesModel)

            _ ->
                Nothing
        , case model.sameObjectAndKeyPropertiesModel of
            Just sameObjectAndKeyPropertiesModel ->
                Just <|
                    Sub.map
                        SameObjectAndKeyPropertiesMsg
                        (Properties.SameObjectAndKey.State.subscriptions sameObjectAndKeyPropertiesModel)

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
                        ( updatedArgumentsModel, childCmd ) =
                            DebateProperties.Index.State.update childMsg debatePropertiesModel
                    in
                        ( { model | activeTab = DebatePropertiesTab updatedArgumentsModel }
                        , Cmd.map translateDebatePropertiesMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        Retrieve ->
            ( { model
                | httpError = Nothing
                , toolbarModel = Nothing
                , typedValue = Nothing
              }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        SameObjectAndKeyPropertiesMsg childMsg ->
            case model.sameObjectAndKeyPropertiesModel of
                Just sameObjectAndKeyPropertiesModel ->
                    let
                        ( updatedSameObjectAndKeyPropertiesModel, childCmd ) =
                            Properties.SameObjectAndKey.State.update childMsg sameObjectAndKeyPropertiesModel
                    in
                        ( { model | sameObjectAndKeyPropertiesModel = Just updatedSameObjectAndKeyPropertiesModel }
                        , Cmd.map translateSameObjectAndKeyPropertiesMsg childCmd
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
                ( mergedModel
                , Ports.setDocumentMetadataForStatementId mergedModel.language mergedModel.data mergedModel.id
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
                | sameObjectAndKeyPropertiesModel = Nothing
                , showTrashed = showTrashed
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            DebatePropertiesRoute ->
                let
                    debatePropertiesModel =
                        DebateProperties.Index.State.init authentication language id

                    ( updatedArgumentsModel, updatedArgumentsCmd ) =
                        DebateProperties.Index.State.urlUpdate location debatePropertiesModel
                in
                    { updatedModel
                        | activeTab = DebatePropertiesTab updatedArgumentsModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translateDebatePropertiesMsg updatedArgumentsCmd
                          ]

            DetailsRoute ->
                ( { updatedModel | activeTab = DetailsTab }, updatedCmd )

            PropertiesRoute ->
                ( { updatedModel | activeTab = PropertiesTab }, updatedCmd )

            SameObjectAndKeyPropertiesRoute keyId ->
                let
                    sameObjectAndKeyPropertiesModel =
                        Properties.SameObjectAndKey.State.init authentication language id keyId

                    ( updatedSameObjectAndKeyPropertiesModel, updatedSameObjectAndKeyPropertiesCmd ) =
                        Properties.SameObjectAndKey.State.urlUpdate location sameObjectAndKeyPropertiesModel
                in
                    { updatedModel | sameObjectAndKeyPropertiesModel = Just updatedSameObjectAndKeyPropertiesModel }
                        ! [ updatedCmd
                          , Cmd.map translateSameObjectAndKeyPropertiesMsg updatedSameObjectAndKeyPropertiesCmd
                          ]
