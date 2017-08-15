module Properties.Item.View exposing (..)

import Arguments.New.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Properties.Item.Types exposing (..)
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock, viewRatingPanel, viewRatingToolbar)
import Values.ViewsHelpers exposing (viewValueIdLine, viewValueTypeLine)
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
                    [ case Dict.get property.objectId data.values of
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
                            i [ class "text-warning" ] [ text ("Missing affirmation with ID: " ++ property.objectId) ]
                    , let
                        ballot =
                            Dict.get property.ballotId data.ballots

                        ballotRating =
                            Maybe.map .rating ballot

                        keyLabel =
                            property.keyId

                        -- TODO
                        -- Dict.get property.keyId data.values
                        --     |> Maybe.map (I18n.translate language)
                        --     |> Maybe.withDefault property.keyId
                      in
                        ul [ class "list-group" ]
                            [ li [ class "d-flex flex-nowrap justify-content-between list-group-item" ]
                                [ div [ class "align-items-baseline d-flex flex-nowrap" ]
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
                                            , ( "mr-2", True )
                                            ]
                                        ]
                                        []
                                    , div []
                                        [ h1 [] [ text keyLabel ]
                                        ]
                                    , case Dict.get property.valueId data.values of
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
                                            i [ class "text-warning" ] [ text ("Missing affirmation with ID: " ++ property.objectId) ]
                                    ]
                                ]
                            ]
                    , viewRatingToolbar
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
