module Properties.Item.View exposing (..)

import Arguments.New.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import LineViews exposing (keyIdLabelCouples, viewStatementIdLine)
import Properties.Item.Types exposing (..)
import Statements.Toolbar.View
import Statements.ViewsHelpers
    exposing
        ( viewDebatePropertiesBlock
        , viewStatementIdRatingPanel
        , viewStatementRatingPanel
        )
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        case ( model.property, model.debatePropertyIds, model.toolbarModel ) of
            ( Just property, Just debatePropertyIds, Just toolbarModel ) ->
                div []
                    [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                        [ div [ class "w-100" ]
                            [ div [ class "align-items-center d-flex flex-nowrap justify-content-between ml-4" ]
                                [ div [ class "lead" ]
                                    [ viewStatementIdLine
                                        language
                                        (Just (ForParent << Navigate))
                                        True
                                        data
                                        property.objectId
                                    ]
                                , viewStatementIdRatingPanel
                                    language
                                    (ForParent << Navigate)
                                    data
                                    property.objectId
                                ]
                            , let
                                keyLabel =
                                    Dict.get property.keyId (Dict.fromList keyIdLabelCouples)
                                        |> Maybe.map (I18n.translate language)
                                        |> Maybe.withDefault property.keyId
                              in
                                div [ class "align-items-baseline d-flex flex-nowrap" ]
                                    [ span
                                        [ ariaHidden True
                                        , classList
                                            [ ( "fa", True )
                                            , ( if property.keyId == "cons" then
                                                    "fa-minus"
                                                else if property.keyId == "pros" then
                                                    "fa-plus"
                                                else
                                                    "fa-circle"
                                              , True
                                              )
                                            , ( "fa-fw", True )
                                            , ( "mr-2", True )
                                            ]
                                        ]
                                        []
                                    , span [ class "lead" ] [ text keyLabel ]
                                    ]
                            , div [ class "align-items-center d-flex flex-nowrap justify-content-between ml-4" ]
                                [ div [ class "lead" ]
                                    [ viewStatementIdLine
                                        language
                                        (Just (ForParent << Navigate))
                                        True
                                        data
                                        property.valueId
                                    ]
                                , viewStatementIdRatingPanel
                                    language
                                    (ForParent << Navigate)
                                    data
                                    property.valueId
                                ]
                            ]
                        , viewStatementRatingPanel language (ForParent << Navigate) Nothing property
                        ]
                    , Statements.Toolbar.View.view toolbarModel
                        |> Html.map translateToolbarMsg
                    , hr [] []
                    , viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
                    , hr [] []
                    , Arguments.New.View.view model.newArgumentModel
                        |> Html.map translateNewArgumentMsg
                    ]

            ( _, _, _ ) ->
                case model.httpError of
                    Just httpError ->
                        div
                            [ class "alert alert-danger"
                            , role "alert"
                            ]
                            [ strong []
                                [ text <|
                                    I18n.translate language I18n.ArgumentsRetrievalFailed
                                        ++ I18n.translate language I18n.Colon
                                ]
                            , text <| Http.Error.toString language httpError
                            ]

                    Nothing ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]
