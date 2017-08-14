module Arguments.Item.View exposing (..)

import Arguments.New.View
import Arguments.Item.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock, viewRatingPanel, viewRatingToolbar)
import Values.ViewsHelpers exposing (viewValueIdLine, viewValueTypeLine)
import Views


keyIdLabelCouples : List ( String, I18n.TranslationId )
keyIdLabelCouples =
    [ ( "pros", I18n.DebateArgumentFor )
    , ( "cons", I18n.DebateArgumentAgainst )
    ]


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        case ( Dict.get model.id data.properties, model.propertyIds ) of
            ( Just debateProperty, Just propertyIds ) ->
                div []
                    [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                        [ div []
                            [ case Dict.get debateProperty.objectId data.values of
                                Just typedValue ->
                                    div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                                        [ h1 []
                                            [ viewValueTypeLine
                                                language
                                                (Just (ForParent << Navigate))
                                                data
                                                False
                                                typedValue.value
                                            ]
                                        , viewRatingPanel language (ForParent << Navigate) (Just "affirmations") typedValue
                                        ]

                                Nothing ->
                                    i [ class "text-warning" ] [ text ("Missing affirmation with ID: " ++ debateProperty.objectId) ]
                            , let
                                keyLabel =
                                    Dict.get debateProperty.keyId (Dict.fromList keyIdLabelCouples)
                                        |> Maybe.map (I18n.translate language)
                                        |> Maybe.withDefault debateProperty.keyId
                              in
                                div [ class "align-items-baseline d-flex flex-nowrap" ]
                                    [ span
                                        [ ariaHidden True
                                        , classList
                                            [ ( "fa", True )
                                            , ( if debateProperty.keyId == "cons" then
                                                    "fa-minus"
                                                else if debateProperty.keyId == "pros" then
                                                    "fa-plus"
                                                else
                                                    "fa-info"
                                              , True
                                              )
                                            , ( "mr-2", True )
                                            ]
                                        ]
                                        []
                                    , h1 [] [ text keyLabel ]
                                    ]
                            , case Dict.get debateProperty.valueId data.values of
                                Just typedValue ->
                                    div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                                        [ h1 []
                                            [ viewValueTypeLine
                                                language
                                                (Just (ForParent << Navigate))
                                                data
                                                False
                                                typedValue.value
                                            ]
                                        , viewRatingPanel language (ForParent << Navigate) (Just "affirmations") typedValue
                                        ]

                                Nothing ->
                                    i [ class "text-warning" ] [ text ("Missing affirmation with ID: " ++ debateProperty.valueId) ]
                            ]
                        , viewRatingPanel language (ForParent << Navigate) Nothing debateProperty
                        ]
                    , viewRatingToolbar
                        language
                        data
                        (\id rating -> ForSelf <| Rate id rating)
                        (ForSelf << Trash)
                        debateProperty
                    , hr [] []
                    , viewDebatePropertiesBlock language (ForParent << Navigate) data propertyIds
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
