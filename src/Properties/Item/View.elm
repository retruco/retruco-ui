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
import Statements.ViewsHelpers
    exposing
        ( viewDebatePropertiesBlock
        , viewStatementIdRatingPanel
        , viewStatementRatingPanel
        , viewStatementRatingToolbar
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
        case ( Dict.get model.id data.properties, model.debatePropertyIds ) of
            ( Just property, Just debatePropertyIds ) ->
                div []
                    [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                        [ div []
                            [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                                [ h1 []
                                    [ viewStatementIdLine
                                        language
                                        (Just (ForParent << Navigate))
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
                                                    "fa-info"
                                              , True
                                              )
                                            , ( "fa-fw", True )
                                            , ( "mr-2", True )
                                            ]
                                        ]
                                        []
                                    , h1 [] [ text keyLabel ]
                                    ]
                            , div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                                [ h1 []
                                    [ viewStatementIdLine
                                        language
                                        (Just (ForParent << Navigate))
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
                    , viewStatementRatingToolbar
                        language
                        data
                        (\id rating -> ForSelf <| Rate id rating)
                        (ForSelf << Trash)
                        property
                    , hr [] []
                    , viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
                    , hr [] []
                    , Arguments.New.View.view model.newArgumentModel
                        |> Html.map translateNewArgumentMsg
                    ]

            ( _, _ ) ->
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
