module Cards.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.New.Types exposing (..)
import Dict exposing (Dict)
import Http
import I18n
import Navigation
import Ports
import Requests
import Task
import Types exposing (DataProxy, Field(..), initDataId, mergeData)
import Urls
import Values.Autocomplete.State


convertControls : Model -> Model
convertControls model =
    let
        language =
            model.language

        ( nameField, errorsList ) =
            case model.namesAutocompleteModel.selected of
                Just nameAutocompletion ->
                    ( Just (CardIdField nameAutocompletion.value.id)
                    , []
                    )

                Nothing ->
                    let
                        ( languageId, languageError ) =
                            if String.isEmpty model.languageId then
                                ( Nothing, Nothing )
                            else
                                case I18n.languageFromLanguageId model.languageId of
                                    Just _ ->
                                        ( Just model.languageId, Nothing )

                                    Nothing ->
                                        ( Nothing, Just ( "language", Just I18n.UnknownLanguage ) )

                        trimmedName =
                            String.trim model.namesAutocompleteModel.autocomplete

                        ( name, nameError ) =
                            if String.isEmpty trimmedName then
                                ( Nothing, Just ( "value", Just I18n.MissingName ) )
                            else
                                ( Just trimmedName, Nothing )
                    in
                        case ( languageError, name, nameError ) of
                            ( Nothing, Just name, Nothing ) ->
                                if String.contains "\n" name || String.contains "\x0D" name then
                                    ( Just (TextareaField languageId name), [] )
                                else
                                    ( Just (InputTextField languageId name), [] )

                            _ ->
                                ( Nothing, List.filterMap identity [ languageError, nameError ] )
    in
        { model
            | errors =
                case nameField of
                    Just _ ->
                        Dict.empty

                    Nothing ->
                        (Dict.fromList <|
                            List.filterMap
                                (\( key, error ) ->
                                    case error of
                                        Just error ->
                                            Just ( key, error )

                                        Nothing ->
                                            Nothing
                                )
                                errorsList
                        )
            , nameField = nameField
        }


init : Maybe Authentication -> Bool -> I18n.Language -> Model
init authentication embed language =
    { authentication = authentication
    , cardId = ""
    , data = initDataId
    , embed = embed
    , errors = Dict.empty
    , httpError = Nothing
    , language = language
    , languageId = I18n.languageIdFromLanguage language
    , name = ""
    , nameField = Nothing
    , nameId = ""
    , namesAutocompleteModel = Values.Autocomplete.State.init [ "schema:string" ] []
    }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    let
        mergedData =
            mergeData data model.data
    in
        { model
            | data = mergedData
        }


setContext : Maybe Authentication -> Bool -> I18n.Language -> Model -> Model
setContext authentication embed language model =
    { model
        | authentication = authentication
        , embed = embed
        , language = language
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.none


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LanguageChanged languageId ->
            ( convertControls
                { model
                    | languageId = languageId
                }
            , Cmd.none
            )

        NameChanged name ->
            ( convertControls { model | name = name }
            , Cmd.none
            )

        NameLocalizationPropertyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        NameLocalizationPropertyUpserted (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model
            in
                ( mergedModel, Cmd.none )

        NamePropertyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        NamePropertyUpserted (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model

                mergedData =
                    mergedModel.data
            in
                -- Reset fields.
                ( { mergedModel
                    | cardId = ""
                    , errors = Dict.empty
                    , httpError = Nothing
                    , name = ""
                    , nameField = Nothing
                    , nameId = ""
                  }
                , Task.perform (\_ -> ForParent <| CardUpserted { mergedData | id = model.cardId }) (Task.succeed ())
                )

        NamesAutocompleteMsg childMsg ->
            let
                ( namesAutocompleteModel, childCmd ) =
                    Values.Autocomplete.State.update
                        childMsg
                        model.authentication
                        model.language
                        "value"
                        model.namesAutocompleteModel
            in
                ( convertControls { model | namesAutocompleteModel = namesAutocompleteModel }
                , Cmd.map translateNamesAutocompleteMsg childCmd
                )

        NameUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        NameUpserted (Ok body) ->
            let
                mergedModel =
                    mergeModelData body.data model

                nameId =
                    body.data.id

                postLocalizationPropertyCmds =
                    case model.nameField of
                        Just (InputTextField (Just languageId) _) ->
                            -- Add property stating that the name is its own localization in given language.
                            [ Requests.postProperty model.authentication nameId languageId nameId (Just 1)
                                |> Http.send (ForSelf << NameLocalizationPropertyUpserted)
                            ]

                        Just (TextareaField (Just languageId) _) ->
                            -- Add property stating that the name is its own localization in given language.
                            [ Requests.postProperty model.authentication nameId languageId nameId (Just 1)
                                |> Http.send (ForSelf << NameLocalizationPropertyUpserted)
                            ]

                        _ ->
                            []
            in
                { mergedModel | nameId = nameId }
                    ! (postLocalizationPropertyCmds
                        ++ [ case model.authentication of
                                Just authentication ->
                                    Requests.postCard authentication
                                        |> Http.send (ForSelf << Upserted)

                                Nothing ->
                                    Task.perform
                                        (\_ -> ForParent <| RequireSignIn <| Submit)
                                        (Task.succeed ())
                           ]
                      )

        Submit ->
            let
                newModel =
                    convertControls model
            in
                ( newModel
                , case newModel.nameField of
                    Just nameField ->
                        case model.authentication of
                            Just authentication ->
                                Requests.postValue
                                    authentication
                                    nameField
                                    |> Http.send (ForSelf << NameUpserted)

                            Nothing ->
                                Task.perform
                                    (\_ -> ForParent <| RequireSignIn <| Submit)
                                    (Task.succeed ())

                    Nothing ->
                        Cmd.none
                )

        Upserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        Upserted (Ok body) ->
            let
                cardId =
                    body.data.id

                mergedModel =
                    mergeModelData body.data model
            in
                ( { mergedModel | cardId = cardId }
                , Requests.postProperty model.authentication cardId "name" model.nameId (Just 1)
                    |> Http.send (ForSelf << NamePropertyUpserted)
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewCardDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewCard
            }
        )
