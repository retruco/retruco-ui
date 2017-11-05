module Cards.New.View exposing (..)

import Cards.New.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http.Error
import I18n
import Json.Decode
import Values.Autocomplete.View
import Views exposing (errorInfos)


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewCard ]
        , viewForm I18n.Create I18n.CardCreationFailed model
        ]


viewForm : I18n.TranslationId -> I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n creationFailedI18n model =
    let
        language =
            model.language

        alert =
            case model.httpError of
                Nothing ->
                    []

                Just httpError ->
                    [ div
                        [ class "alert alert-danger"
                        , role "alert"
                        ]
                        [ strong []
                            [ text <|
                                I18n.translate language creationFailedI18n
                                    ++ I18n.translate language I18n.Colon
                            ]
                        , text <| Http.Error.toString language httpError
                        ]
                    ]
    in
        Html.form
            [ onSubmit (ForSelf <| Submit) ]
            (alert
                ++ (viewFormControls model)
                ++ [ button
                        [ class "btn btn-primary"
                        , disabled (model.nameField == Nothing)
                        , type_ "submit"
                        ]
                        [ text (I18n.translate language submitButtonI18n) ]
                   ]
            )


viewFormControls : Model -> List (Html Msg)
viewFormControls model =
    let
        language =
            model.language
    in
        [ let
            controlId =
                "value"
          in
            Values.Autocomplete.View.viewAutocomplete
                language
                controlId
                I18n.Value
                I18n.ValuePlaceholder
                (Dict.get controlId model.errors)
                model.namesAutocompleteModel
                |> Html.map translateNamesAutocompleteMsg
        , let
            controlId =
                "language"

            ( errorClass, errorAttributes, errorBlock ) =
                errorInfos language controlId (Dict.get controlId model.errors)
          in
            div [ class ("form-group" ++ errorClass) ]
                ([ label
                    [ class "control-label sr-only", for controlId ]
                    [ text <| I18n.translate language I18n.LanguageWord ]
                 , select
                    ([ class "form-control custom-select"
                     , id controlId
                     , on "change"
                        (Json.Decode.map (ForSelf << LanguageChanged)
                            targetValue
                        )
                     ]
                        ++ errorAttributes
                    )
                    (I18n.languages
                        |> List.map
                            (\languageI18n ->
                                ( I18n.languageIdFromLanguage languageI18n
                                , I18n.translate language (I18n.Language languageI18n)
                                )
                            )
                        |> List.sortBy (\( languageId, languageLabel ) -> languageLabel)
                        |> (::) ( "", I18n.translate language I18n.EveryLanguage )
                        |> List.map
                            (\( languageId, languageLabel ) ->
                                option
                                    [ selected (languageId == model.languageId)
                                    , value languageId
                                    ]
                                    [ text languageLabel ]
                            )
                    )
                 ]
                    ++ errorBlock
                )
        ]
