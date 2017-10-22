module Values.New.State exposing (..)

import Authenticator.Types exposing (Authentication)
import Cards.Autocomplete.State
import Dict exposing (Dict)
import Http
import Http.Error
import I18n
import Image.Types exposing (..)
import Navigation
import Ports
import Requests
import Task
import Types exposing (DataProxy, Field(..))
import Urls
import Values.Autocomplete.State
import Values.New.Types exposing (..)


convertControls : Model -> Model
convertControls model =
    let
        language =
            model.language

        ( field, errorsList ) =
            case model.fieldType of
                "" ->
                    ( Nothing
                    , [ ( "fieldType", Just I18n.MissingValue ) ]
                    )

                "BooleanField" ->
                    ( Just (BooleanField model.booleanValue)
                    , []
                    )

                "CardIdField" ->
                    case model.cardsAutocompleteModel.selected of
                        Just cardAutocompletion ->
                            ( Just (CardIdField cardAutocompletion.card.id)
                            , []
                            )

                        Nothing ->
                            ( Nothing
                            , [ ( "cardId", Just I18n.MissingValue ) ]
                            )

                "ImageField" ->
                    case model.imageUploadStatus of
                        ImageNotUploadedStatus ->
                            ( Nothing
                            , [ ( "new-image", Just I18n.UploadImage ) ]
                            )

                        ImageSelectedStatus ->
                            ( Nothing
                            , [ ( "new-image", Just I18n.ReadingSelectedImage ) ]
                            )

                        ImageReadStatus { contents, filename } ->
                            ( Nothing
                            , [ ( "new-image", Just (I18n.UploadingImage filename) ) ]
                            )

                        ImageUploadedStatus path ->
                            ( Just (ImageField path)
                            , []
                            )

                        ImageUploadErrorStatus httpError ->
                            ( Nothing
                            , [ ( "new-image", Just (I18n.ImageUploadError (Http.Error.toString language httpError)) ) ]
                            )

                "InputEmailField" ->
                    case String.trim model.value of
                        "" ->
                            ( Nothing
                            , [ ( "value", Just I18n.MissingValue ) ]
                            )

                        email ->
                            ( Just (InputEmailField email)
                            , []
                            )

                "InputNumberField" ->
                    case String.trim model.value of
                        "" ->
                            ( Nothing
                            , [ ( "value", Just I18n.MissingValue ) ]
                            )

                        value ->
                            case String.toFloat value of
                                Err _ ->
                                    ( Nothing
                                    , [ ( "value", Just I18n.InvalidNumber ) ]
                                    )

                                Ok number ->
                                    ( Just (InputNumberField number)
                                    , []
                                    )

                "InputUrlField" ->
                    case String.trim model.value of
                        "" ->
                            ( Nothing
                            , [ ( "value", Just I18n.MissingValue ) ]
                            )

                        url ->
                            ( Just (InputUrlField url)
                            , []
                            )

                "TextField" ->
                    case model.valuesAutocompleteModel.selected of
                        Just valueAutocompletion ->
                            ( Just (ValueIdField valueAutocompletion.value.id)
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

                                trimmedText =
                                    String.trim model.valuesAutocompleteModel.autocomplete

                                ( text, textError ) =
                                    if String.isEmpty trimmedText then
                                        ( Nothing, Just ( "value", Just I18n.MissingValue ) )
                                    else
                                        ( Just trimmedText, Nothing )
                            in
                                case ( languageError, text, textError ) of
                                    ( Nothing, Just text, Nothing ) ->
                                        if String.contains "\n" text || String.contains "\x0D" text then
                                            ( Just (TextareaField languageId text), [] )
                                        else
                                            ( Just (InputTextField languageId text), [] )

                                    _ ->
                                        ( Nothing, List.filterMap identity [ languageError, textError ] )

                _ ->
                    ( Nothing
                    , [ ( "fieldType", Just I18n.UnknownValue ) ]
                    )
    in
        { model
            | errors =
                case field of
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
            , field = field
        }


init : Maybe Authentication -> I18n.Language -> List String -> Model
init authentication language validFieldTypes =
    let
        fieldType =
            case List.head validFieldTypes of
                Just firstFieldType ->
                    if List.member "TextField" validFieldTypes then
                        "TextField"
                    else
                        firstFieldType

                Nothing ->
                    "TextField"

        languageId =
            I18n.languageIdFromLanguage language

        ( schemaIds, widgetIds ) =
            schemaIdsAndWidgetIds fieldType languageId
    in
        { authentication = authentication
        , booleanValue = False
        , cardsAutocompleteModel = Cards.Autocomplete.State.init []
        , errors = Dict.empty
        , field = Nothing
        , fieldType = fieldType
        , httpError = Nothing
        , imageUploadStatus = ImageNotUploadedStatus
        , language = language
        , languageId = languageId
        , validFieldTypes = validFieldTypes
        , value = ""
        , valuesAutocompleteModel = Values.Autocomplete.State.init schemaIds widgetIds
        }


mergeModelData : DataProxy a -> Model -> Model
mergeModelData data model =
    model


schemaIdsAndWidgetIds : String -> String -> ( List String, List String )
schemaIdsAndWidgetIds fieldType languageId =
    case fieldType of
        "BooleanField" ->
            ( [ "schema:boolean" ], [] )

        "CardIdField" ->
            ( [ "schema:card-id" ], [] )

        "ImageField" ->
            ( [ "schema:uri-reference" ], [ "widget:image" ] )

        "InputEmailField" ->
            ( [ "schema:email" ], [] )

        "InputNumberField" ->
            ( [ "schema:number" ], [] )

        "InputUrlField" ->
            ( [ "schema:uri" ], [] )

        "TextField" ->
            ( [ "schema:string" ], [] )

        _ ->
            ( [], [] )


setContext : Maybe Authentication -> I18n.Language -> Model -> Model
setContext authentication language model =
    { model
        | authentication = authentication
        , language = language
    }


subscriptions : Model -> Sub InternalMsg
subscriptions model =
    Sub.batch
        [ Sub.map CardsAutocompleteMsg (Cards.Autocomplete.State.subscriptions model.cardsAutocompleteModel)
        , Ports.fileContentRead ImageRead
        ]


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardsAutocompleteMsg childMsg ->
            let
                ( cardsAutocompleteModel, childCmd ) =
                    Cards.Autocomplete.State.update
                        childMsg
                        model.authentication
                        model.language
                        "cardId"
                        model.cardsAutocompleteModel
            in
                ( convertControls { model | cardsAutocompleteModel = cardsAutocompleteModel }
                , Cmd.map translateCardsAutocompleteMsg childCmd
                )

        FieldTypeChanged fieldType ->
            let
                ( schemaIds, widgetIds ) =
                    schemaIdsAndWidgetIds fieldType model.languageId
            in
                ( convertControls
                    { model
                        | fieldType = fieldType
                        , valuesAutocompleteModel =
                            Values.Autocomplete.State.setSchemaIdsAndWidgetIds
                                schemaIds
                                widgetIds
                                model.valuesAutocompleteModel
                    }
                , Cmd.none
                )

        ImageRead data ->
            let
                newModel =
                    convertControls { model | imageUploadStatus = ImageReadStatus data }

                -- Ensure that image is uploaded only once, although port "fileContentRead" is sent to every image
                -- editor.
                cmd =
                    case model.imageUploadStatus of
                        ImageNotUploadedStatus ->
                            Cmd.none

                        ImageSelectedStatus ->
                            Requests.postUploadImage newModel.authentication data.contents
                                |> Http.send ImageUploaded
                                |> Cmd.map ForSelf

                        ImageReadStatus _ ->
                            Cmd.none

                        ImageUploadedStatus _ ->
                            Cmd.none

                        ImageUploadErrorStatus _ ->
                            Cmd.none
            in
                ( newModel, cmd )

        ImageSelected ->
            ( convertControls { model | imageUploadStatus = ImageSelectedStatus }
            , Ports.fileSelected "new-image"
            )

        ImageUploaded (Err err) ->
            ( convertControls { model | imageUploadStatus = ImageUploadErrorStatus err }, Cmd.none )

        ImageUploaded (Ok path) ->
            ( convertControls { model | imageUploadStatus = ImageUploadedStatus path }
            , Cmd.none
            )

        LanguageChanged languageId ->
            let
                ( schemaIds, widgetIds ) =
                    schemaIdsAndWidgetIds model.fieldType languageId
            in
                ( convertControls
                    { model
                        | languageId = languageId
                        , valuesAutocompleteModel =
                            Values.Autocomplete.State.setSchemaIdsAndWidgetIds
                                schemaIds
                                widgetIds
                                model.valuesAutocompleteModel
                    }
                , Cmd.none
                )

        LocalizationPropertyUpserted (Err httpError) ->
            ( { model | httpError = Just httpError }, Cmd.none )

        LocalizationPropertyUpserted (Ok body) ->
            ( model, Cmd.none )

        Submit ->
            let
                newModel =
                    convertControls model
            in
                ( newModel
                , case newModel.field of
                    Just field ->
                        case model.authentication of
                            Just authentication ->
                                Requests.postValue
                                    authentication
                                    field
                                    |> Http.send (ForSelf << Upserted)

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
            -- Reset fields.
            let
                ( schemaIds, widgetIds ) =
                    schemaIdsAndWidgetIds model.fieldType model.languageId

                postLocalizationPropertyCmds =
                    case model.field of
                        Just (InputTextField (Just languageId) _) ->
                            -- Add property stating that the text is its own localization in given language.
                            let
                                textId =
                                    body.data.id
                            in
                                [ Requests.postProperty model.authentication textId languageId textId (Just 1)
                                    |> Http.send (ForSelf << LocalizationPropertyUpserted)
                                ]

                        Just (TextareaField (Just languageId) _) ->
                            -- Add property stating that the text is its own localization in given language.
                            let
                                textId =
                                    body.data.id
                            in
                                [ Requests.postProperty model.authentication textId languageId textId (Just 1)
                                    |> Http.send (ForSelf << LocalizationPropertyUpserted)
                                ]

                        _ ->
                            []
            in
                { model
                    | booleanValue = False
                    , cardsAutocompleteModel = Cards.Autocomplete.State.init []
                    , errors = Dict.empty
                    , field = Nothing
                    , httpError = Nothing
                    , imageUploadStatus = ImageNotUploadedStatus
                    , value = ""
                    , valuesAutocompleteModel = Values.Autocomplete.State.init schemaIds widgetIds
                }
                    ! (postLocalizationPropertyCmds
                        ++ [ Task.perform (\_ -> ForParent <| ValueUpserted body.data) (Task.succeed ()) ]
                      )

        ValueChanged value ->
            ( convertControls { model | value = value }
            , Cmd.none
            )

        ValueChecked booleanValue ->
            ( convertControls { model | booleanValue = booleanValue }
            , Cmd.none
            )

        ValuesAutocompleteMsg childMsg ->
            let
                ( valuesAutocompleteModel, childCmd ) =
                    Values.Autocomplete.State.update
                        childMsg
                        model.authentication
                        model.language
                        "value"
                        model.valuesAutocompleteModel
            in
                ( convertControls { model | valuesAutocompleteModel = valuesAutocompleteModel }
                , Cmd.map translateValuesAutocompleteMsg childCmd
                )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.NewValueDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.NewValue
            }
        )
