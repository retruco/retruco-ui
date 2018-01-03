module Properties.SameObject.State exposing (..)

import Array
import Authenticator.Types exposing (Authentication)
import Data exposing (initData, mergeData)
import Http
import I18n
import Navigation
import Ports
import Properties.KeysAutocomplete.State
import Properties.SameObject.Types exposing (..)
import Requests
import Task
import Types exposing (..)
import Urls


init : Maybe Authentication -> Bool -> I18n.Language -> String -> Model
init authentication embed language objectId =
    { authentication = authentication
    , data = initData
    , embed = embed
    , httpError = Nothing
    , keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
    , language = language
    , objectId = objectId
    , propertyIds = Nothing
    , showTrashed = False
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    { model
        | data = mergeData data model.data
    }


propagateModelDataChange : Model -> Model
propagateModelDataChange model =
    model


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map KeysAutocompleteMsg (Properties.KeysAutocomplete.State.subscriptions model.keysAutocompleteModel)
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddKey typedValue ->
            ( model
            , Task.perform
                (\_ ->
                    ForParent <|
                        Navigate <|
                            Urls.languagePath model.embed model.language <|
                                Urls.idToSameObjectAndKeyPropertiesPath model.data model.objectId typedValue.id
                )
                (Task.succeed ())
            )

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
            let
                mergedModel =
                    mergeModelData data model
                        |> propagateModelDataChange
            in
                ( mergedModel
                , Task.perform
                    (\_ ->
                        ForParent <|
                            Navigate <|
                                Urls.languagePath mergedModel.embed mergedModel.language <|
                                    Urls.idToSameObjectAndKeyPropertiesPath mergedModel.data model.objectId data.id
                    )
                    (Task.succeed ())
                )

        Retrieve ->
            ( { model
                | propertyIds = Nothing
                , httpError = Nothing
              }
            , Requests.getProperties model.authentication model.showTrashed [ model.objectId ] [] []
                |> Http.send (ForSelf << Retrieved)
            )

        Retrieved (Err httpError) ->
            ( { model
                | httpError = Just httpError
                , keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
              }
            , Cmd.none
            )

        Retrieved (Ok { data }) ->
            let
                mergedModel =
                    mergeModelData data model
                        |> propagateModelDataChange

                language =
                    model.language
            in
                ( { mergedModel
                    -- | keysAutocompleteModel = Properties.KeysAutocomplete.State.init card.subTypeIds True
                    | keysAutocompleteModel = Properties.KeysAutocomplete.State.init [] True
                    , propertyIds = Just data.ids
                  }
                , Ports.setDocumentMetadata
                    { description = I18n.translate language I18n.PropertiesDescription
                    , imageUrl = Urls.appLogoFullUrl
                    , title = I18n.translate language I18n.Properties
                    }
                )

        Upserted data ->
            let
                mergedModel =
                    mergeModelData data model
                        |> propagateModelDataChange

                language =
                    model.language
            in
                ( { mergedModel
                    | propertyIds =
                        case model.propertyIds of
                            Just propertyIds ->
                                if List.member data.id <| Array.toList propertyIds then
                                    Just propertyIds
                                else
                                    Just <| Array.append (Array.fromList [ data.id ]) propertyIds

                            Nothing ->
                                Just <| Array.fromList [ data.id ]
                  }
                , Cmd.none
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    update Retrieve
        { model
            | showTrashed = Urls.queryToggle "trashed" location
        }
