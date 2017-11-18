module Interventions.New.View exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Http.Error
import I18n
import Interventions.New.Types exposing (..)
import Json.Decode
import Proposals.New.View
import Views exposing (errorInfos)


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text <| I18n.translate model.language I18n.NewIntervention ]
        , viewForm I18n.Create I18n.InterventionCreationFailed model
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
                        , disabled
                            (not (Dict.isEmpty model.errors)
                                || model.newProposalModel.newValueModel.field
                                == Nothing
                            )
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
        (Proposals.New.View.viewFormControls model.newProposalModel
            |> List.map (Html.map translateNewProposalMsg)
        )
            ++ (if List.length model.keyIds <= 1 then
                    []
                else
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
                                (I18n.discussionKeyIdLabelCouples
                                    |> List.filter
                                        (\( keyId, _ ) -> List.member keyId model.keyIds)
                                    |> List.map
                                        (\( keyId, labelI18n ) ->
                                            ( keyId
                                            , I18n.translate language labelI18n
                                            )
                                        )
                                    |> List.sortBy (\( keyId, label ) -> label)
                                    |> List.map
                                        (\( keyId, label ) ->
                                            option
                                                [ selected (keyId == model.keyId)
                                                , value keyId
                                                ]
                                                [ text label ]
                                        )
                                )
                             ]
                                ++ errorBlock
                            )
                    ]
               )
