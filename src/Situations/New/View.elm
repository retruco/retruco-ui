module Situations.New.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http.Error
import I18n
import Situations.New.Types exposing (..)
import Values.New.View


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewSituation ]
        , viewForm I18n.Create model
        ]


viewForm : I18n.TranslationId -> Model -> Html Msg
viewForm submitButtonI18n model =
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
                                I18n.translate language I18n.ValueCreationFailed
                                    ++ I18n.translate language I18n.Colon
                            ]
                        , text <| Http.Error.toString language httpError
                        ]
                    ]
    in
        Html.form
            [ onSubmit (ForSelf <| Submit) ]
            (alert
                ++ viewFormControls model
                ++ [ button
                        [ class "btn btn-primary"
                        , disabled (model.newValueModel.field == Nothing)
                        , type_ "submit"
                        ]
                        [ text (I18n.translate language submitButtonI18n) ]
                   ]
            )


viewFormControls : Model -> List (Html Msg)
viewFormControls model =
    Values.New.View.viewFormControls model.newValueModel
        |> List.map (Html.map translateNewValueMsg)
