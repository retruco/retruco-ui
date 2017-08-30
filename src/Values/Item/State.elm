module Values.Item.State exposing (..)

import Arguments.Index.State
import Authenticator.Types exposing (Authentication)
import Constants exposing (debateKeyIds)
import Http
import I18n
import Navigation
import Ports
import Requests
import Types exposing (..)
import Urls
import Values.Item.Routes exposing (..)
import Values.Item.Types exposing (..)


init : Maybe Authentication -> I18n.Language -> String -> Model
init authentication language id =
    { argumentsModel = Nothing
    , authentication = authentication
    , data = initData
    , debatePropertyIds = Nothing
    , httpError = Nothing
    , id = id
    , language = language

    -- , sameKeyPropertiesModel = Nothing
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

            -- , sameKeyPropertiesModel =
            --     case model.sameKeyPropertiesModel of
            --         Just sameKeyPropertiesModel ->
            --             Just <| SameKeyProperties.State.mergeModelData mergedData sameKeyPropertiesModel
            --         Nothing ->
            --             Nothing
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
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    List.filterMap identity
        [ case model.argumentsModel of
            Just argumentsModel ->
                Just <| Sub.map ArgumentsMsg (Arguments.Index.State.subscriptions argumentsModel)

            Nothing ->
                Nothing

        -- , Just <|
        --     Sub.map KeysAutocompleteMsg (Properties.KeysAutocomplete.State.subscriptions model.keysAutocompleteModel)
        -- , case model.sameKeyPropertiesModel of
        --     Just sameKeyPropertiesModel ->
        --         Just <| Sub.map SameKeyPropertiesMsg (SameKeyProperties.State.subscriptions sameKeyPropertiesModel)
        --     Nothing ->
        --         Nothing
        ]
        |> Sub.batch


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Retrieve ->
            ( { model | httpError = Nothing }
            , Requests.getValue model.authentication model.id
                |> Http.send (ForSelf << ValueRetrieved)
            )

        ValueRetrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError

                -- , keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
              }
            , Cmd.none
            )

        ValueRetrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
            in
                -- ( { mergedModel
                --     | keysAutocompleteModel = Properties.KeysAutocomplete.State.init card.subTypeIds True
                --   }
                mergedModel
                    ! [ Ports.setDocumentMetadataForStatementId mergedModel.language mergedModel.data mergedModel.id
                      , Requests.getObjectProperties model.authentication model.showTrashed model.id debateKeyIds []
                            |> Http.send (ForSelf << DebatePropertiesRetrieved)
                      ]


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

                -- , sameKeyPropertiesModel = Nothing
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
