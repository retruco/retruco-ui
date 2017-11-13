module DebateProperties.New.View exposing (..)

import DebateProperties.New.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http.Error
import I18n
import Json.Decode
import Values.New.View
import Views exposing (errorInfos)


view : Model -> Html Msg
view model =
    section []
        [ h4 [] [ text <| I18n.translate model.language I18n.NewArgument ]
        , viewForm I18n.Create I18n.ValueCreationFailed model
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
                        , disabled (not (Dict.isEmpty model.errors) || model.newValueModel.field == Nothing)
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
                "keyId"

            ( errorClass, errorAttributes, errorBlock ) =
                errorInfos language controlId (Dict.get controlId model.errors)
          in
            div [ class ("form-group" ++ errorClass) ]
                ([ label
                    [ class "control-label sr-only", for controlId ]
                    [ text <| I18n.translate language I18n.ArgumentType ]
                 , select
                    ([ class "form-control custom-select"
                     , id controlId
                     , on "change"
                        (Json.Decode.map (ForSelf << KeyIdChanged)
                            targetValue
                        )
                     ]
                        ++ errorAttributes
                    )
                    (I18n.debateKeyIdLabelCouples
                        |> List.map
                            (\( symbol, labelI18n ) ->
                                ( symbol
                                , I18n.translate language labelI18n
                                )
                            )
                        |> List.sortBy (\( symbol, label ) -> label)
                        |> (::) ( "", I18n.translate language I18n.SelectArgumentType )
                        |> List.map
                            (\( symbol, label ) ->
                                option
                                    [ selected (symbol == model.keyId)
                                    , value symbol
                                    ]
                                    [ text label ]
                            )
                    )
                 ]
                    ++ errorBlock
                )
        ]
            ++ (Values.New.View.viewFormControls model.newValueModel
                    |> List.map (Html.map translateNewValueMsg)
               )
