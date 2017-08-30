module Cards.Item.State exposing (..)

import Arguments.Index.State
import Authenticator.Types exposing (Authentication)
import Cards.Item.Routes exposing (..)
import Cards.Item.Types exposing (..)
import Constants exposing (debateKeyIds)
import Http
import I18n
import Navigation
import Ports
import Properties.KeysAutocomplete.State
import Requests
import SameKeyProperties.State
import Task
import Types exposing (..)
import Urls


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { argumentsModel = Nothing
    , authentication = authentication
    , data = initData
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , id = id
    , keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
    , language = language
    , sameKeyPropertiesModel = Nothing
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | argumentsModel =
                case model.argumentsModel of
                    Just argumentsModel ->
                        Just <| Arguments.Index.State.mergeModelData mergedData argumentsModel

                    Nothing ->
                        Nothing
            , data = mergedData
            , sameKeyPropertiesModel =
                case model.sameKeyPropertiesModel of
                    Just sameKeyPropertiesModel ->
                        Just <| SameKeyProperties.State.mergeModelData mergedData sameKeyPropertiesModel

                    Nothing ->
                        Nothing
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | argumentsModel =
            case model.argumentsModel of
                Just argumentsModel ->
                    Just <| Arguments.Index.State.setContext authentication language argumentsModel

                Nothing ->
                    Nothing
        , authentication = authentication
        , language = language
        , sameKeyPropertiesModel =
            case model.sameKeyPropertiesModel of
                Just sameKeyPropertiesModel ->
                    Just <| SameKeyProperties.State.setContext authentication language sameKeyPropertiesModel

                Nothing ->
                    Nothing
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    List.filterMap identity
        [ case model.argumentsModel of
            Just argumentsModel ->
                Just <| Sub.map ArgumentsMsg (Arguments.Index.State.subscriptions argumentsModel)

            Nothing ->
                Nothing
        , Just <|
            Sub.map KeysAutocompleteMsg (Properties.KeysAutocomplete.State.subscriptions model.keysAutocompleteModel)
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
        AddKey typedValue ->
            -- TODO
            -- update (LoadProperties typedValue.id) model
            ( model, Cmd.none )

        ArgumentsMsg childMsg ->
            case model.argumentsModel of
                Just argumentsModel ->
                    let
                        ( updatedArgumentsModel, childCmd ) =
                            Arguments.Index.State.update childMsg argumentsModel
                    in
                        ( { model | argumentsModel = Just updatedArgumentsModel }
                        , Cmd.map translateArgumentsMsg childCmd
                        )

                Nothing ->
                    ( model, Cmd.none )

        CardRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
                , keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
              }
            , Cmd.none
            )

        CardRetrieved (Ok { data }) ->
            let
                card =
                    getCard data.cards data.id

                mergedModel =
                    mergeModelData data model
            in
                { mergedModel
                    | keysAutocompleteModel = Properties.KeysAutocomplete.State.init card.subTypeIds True
                }
                    ! [ Ports.setDocumentMetadataForStatementId mergedModel.language mergedModel.data mergedModel.id
                      , Requests.getObjectProperties model.authentication model.showTrashed model.id debateKeyIds []
                            |> Http.send (ForSelf << DebatePropertiesRetrieved)
                      ]

        CreateKey keyName ->
            case model.authentication of
                Just authentication ->
                    ( model
                    , Requests.postValue
                        authentication
                        (InputTextField Nothing keyName)
                        |> Http.send (ForSelf << KeyUpserted)
                    )

                Nothing ->
                    ( model
                    , Task.perform
                        (\_ -> ForParent <| RequireSignIn <| CreateKey keyName)
                        (Task.succeed ())
                    )

        DebatePropertiesRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
              }
            , Cmd.none
            )

        DebatePropertiesRetrieved (Ok { data }) ->
            let
                language =
                    model.language

                mergedModel =
                    mergeModelData data model
            in
                ( { mergedModel | debatePropertyIds = Just data.ids }, Cmd.none )

        KeysAutocompleteMsg childMsg ->
            let
                ( keysAutocompleteModel, childCmd ) =
                    Properties.KeysAutocomplete.State.update
                        childMsg
                        model.authentication
                        model.language
                        "keyId"
                        model.keysAutocompleteModel
            in
                ( { model | keysAutocompleteModel = keysAutocompleteModel }
                , Cmd.map translateKeysAutocompleteMsg childCmd
                )

        KeyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        KeyUpserted (Ok { data }) ->
            -- let
            --     mergedModel =
            --          mergeModelData data model
            -- in
            --     update (LoadProperties data.id) mergedModel
            ( model, Cmd.none )

        Retrieve ->
            ( { model
                | debatePropertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getCard model.authentication model.id
                |> Http.send (ForSelf << CardRetrieved)
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
                | argumentsModel = Nothing
                , sameKeyPropertiesModel = Nothing
                , showTrashed = Urls.queryToggle "trashed" location
            }

        ( updatedModel, updatedCmd ) =
            update Retrieve unroutedModel
    in
        case route of
            ArgumentsRoute ->
                let
                    argumentsModel =
                        Arguments.Index.State.init authentication language id

                    ( updatedArgumentsModel, updatedArgumentsCmd ) =
                        Arguments.Index.State.urlUpdate location argumentsModel
                in
                    { updatedModel | argumentsModel = Just updatedArgumentsModel }
                        ! [ updatedCmd
                          , Cmd.map translateArgumentsMsg updatedArgumentsCmd
                          ]

            IndexRoute ->
                ( updatedModel, updatedCmd )

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
