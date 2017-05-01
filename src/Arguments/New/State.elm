module Arguments.New.State exposing (..)

import Arguments.New.Types exposing (..)
import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Task
import Types exposing (DataProxy, initDataId, mergeData, mergeDataId)
import Urls
import Values.New.State
import Values.New.Types


init : Maybe Authentication -> I18n.Language -> String -> List String -> Model
init authentication language objectId validFieldTypes =
    { authentication = authentication
    , data = initDataId
    , errors = Dict.empty
    , httpError = Nothing
    , keyId = ""
    , language = language
    , newValueModel = Values.New.State.init authentication language validFieldTypes
    , objectId = objectId
    , validFieldTypes = validFieldTypes
    }


convertControls : Model -> Model
convertControls model =
    let
        keyIdError =
            case model.keyId of
                "" ->
                    Just I18n.MissingValue

                "cons" ->
                    Nothing

                "pros" ->
                    Nothing

                _ ->
                    Just I18n.UnknownValue
    in
        { model
            | errors =
                case keyIdError of
                    Just keyIdError ->
                        Dict.singleton "keyId" keyIdError

                    Nothing ->
                        Dict.empty
        }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
            , newValueModel = Values.New.State.mergeModelData mergedData model.newValueModel
        }


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.map NewValueMsg (Values.New.State.subscriptions model.newValueModel)


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyIdChanged keyId ->
            ( convertControls { model | keyId = keyId }, Cmd.none )

        NewValueMsg childMsg ->
            let
                ( newValueModel, childCmd ) =
                    model.newValueModel
                        |> Values.New.State.setContext model.authentication model.language
                        |> Values.New.State.update childMsg
            in
                ( { model | newValueModel = newValueModel }
                , Cmd.map translateNewValueMsg childCmd
                )

        Submit ->
            let
                newModel =
                    convertControls model
            in
                if Dict.isEmpty newModel.errors then
                    update (NewValueMsg Values.New.Types.Submit) newModel
                else
                    ( newModel, Cmd.none )

        Upserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Upserted (Ok body) ->
            let
                data =
                    mergeDataId body.data model.data
            in
                ( { model | data = data }
                , Task.perform (\_ -> ForParent <| PropertyUpserted data) (Task.succeed ())
                )

        ValueUpserted data ->
            ( { model | data = mergeData data model.data }
            , Requests.postProperty model.authentication model.objectId model.keyId data.id
                |> Http.send (ForSelf << Upserted)
            )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewArgumentDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewArgument
            }
        )
