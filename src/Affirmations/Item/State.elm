module Affirmations.Item.State exposing (..)

import Affirmations.Item.Routes exposing (..)
import Affirmations.Item.Types exposing (..)
import Arguments.Index.State
import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import SameKeyProperties.State
import Statements.Toolbar.State
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { activeTab = PropertiesTab
    , affirmation = Nothing
    , authentication = authentication
    , data = initData
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

        affirmation =
            Dict.get model.id mergedData.values
    in
        { model
            | activeTab =
                case model.activeTab of
                    DebatePropertiesTab argumentsModel ->
                        DebatePropertiesTab <| Arguments.Index.State.mergeModelData mergedData argumentsModel

                    _ ->
                        model.activeTab
            , affirmation = affirmation
            , data = mergedData
            , sameKeyPropertiesModel =
                case model.sameKeyPropertiesModel of
                    Just sameKeyPropertiesModel ->
                        Just <| SameKeyProperties.State.mergeModelData mergedData sameKeyPropertiesModel

                    Nothing ->
                        Nothing
            , toolbarModel =
                case affirmation of
                    Just affirmation ->
                        case model.toolbarModel of
                            Just toolbarModel ->
                                Just <| Statements.Toolbar.State.setData mergedData affirmation toolbarModel

                            Nothing ->
                                Just <|
                                    Statements.Toolbar.State.init
                                        model.authentication
                                        model.language
                                        mergedData
                                        affirmation

                    Nothing ->
                        Nothing
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | activeTab =
            case model.activeTab of
                DebatePropertiesTab argumentsModel ->
                    DebatePropertiesTab <| Arguments.Index.State.setContext authentication language argumentsModel

                _ ->
                    model.activeTab
        , authentication = authentication
        , language = language
        , sameKeyPropertiesModel =
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    Just <| SameKeyProperties.State.setContext authentication language sameKeyPropertiesModel

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
            DebatePropertiesTab argumentsModel ->
                Just <| Sub.map ArgumentsMsg (Arguments.Index.State.subscriptions argumentsModel)

            _ ->
                Nothing
        , case model.sameKeyPropertiesModel of
            Just sameKeyPropertiesModel ->
                Just <| Sub.map SameKeyPropertiesMsg (SameKeyProperties.State.subscriptions sameKeyPropertiesModel)

            Nothing ->
                Nothing
        ]
        |> Sub.batch


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataUpdated data ->
            ( mergeModelData data model, Cmd.none )

        ArgumentsMsg childMsg ->
            case model.activeTab of
                DebatePropertiesTab argumentsModel ->
                    let
                        ( updatedArgumentsModel, childCmd ) =
                            Arguments.Index.State.update childMsg argumentsModel
                    in
                        ( { model | activeTab = DebatePropertiesTab updatedArgumentsModel }
                        , Cmd.map translateArgumentsMsg childCmd
                        )

                _ ->
                    ( model, Cmd.none )

        Retrieve ->
            ( { model
                | affirmation = Nothing
                , httpError = Nothing
                , toolbarModel = Nothing
              }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        SameKeyPropertiesMsg childMsg ->
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    let
                        ( updatedSameKeyPropertiesModel, childCmd ) =
                            SameKeyProperties.State.update childMsg sameKeyPropertiesModel
                    in
                        ( { model | sameKeyPropertiesModel = Just updatedSameKeyPropertiesModel }
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
                | sameKeyPropertiesModel = Nothing
                , showTrashed = showTrashed
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            DebatePropertiesRoute ->
                let
                    argumentsModel =
                        Arguments.Index.State.init authentication language id

                    ( updatedArgumentsModel, updatedArgumentsCmd ) =
                        Arguments.Index.State.urlUpdate location argumentsModel
                in
                    { updatedModel
                        | activeTab = DebatePropertiesTab updatedArgumentsModel
                    }
                        ! [ updatedCmd
                          , Cmd.map translateArgumentsMsg updatedArgumentsCmd
                          ]

            DetailsRoute ->
                ( { updatedModel | activeTab = DetailsTab }, updatedCmd )

            PropertiesRoute ->
                ( { updatedModel | activeTab = PropertiesTab }, updatedCmd )

            SameKeyPropertiesRoute keyId ->
                let
                    sameKeyPropertiesModel =
                        SameKeyProperties.State.init authentication language id keyId

                    ( updatedSameKeyPropertiesModel, updatedSameKeyPropertiesCmd ) =
                        SameKeyProperties.State.urlUpdate location sameKeyPropertiesModel
                in
                    { updatedModel | sameKeyPropertiesModel = Just updatedSameKeyPropertiesModel }
                        ! [ updatedCmd
                          , Cmd.map translateSameKeyPropertiesMsg updatedSameKeyPropertiesCmd
                          ]
