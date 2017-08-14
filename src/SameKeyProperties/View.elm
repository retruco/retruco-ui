module SameKeyProperties.View exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import SameKeyProperties.Types exposing (..)
import Statements.ViewsHelpers exposing (viewRatingPanel)
import Values.New.View
import Values.ViewsHelpers exposing (viewValueIdLine)
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        case model.propertyIds of
            Just propertyIds ->
                div []
                    [ ul [ class "list-group" ]
                        (List.filterMap
                            (\propertyId ->
                                case Dict.get propertyId data.properties of
                                    Just property ->
                                        Just <|
                                            li [ class "d-flex flex-nowrap justify-content-between list-group-item" ]
                                                [ viewValueIdLine language Nothing data False property.valueId
                                                , viewRatingPanel language (ForParent << Navigate) Nothing property
                                                ]

                                    Nothing ->
                                        Nothing
                            )
                            propertyIds
                        )
                    , Values.New.View.view model.newValueModel
                        |> Html.map translateNewValueMsg
                    ]

            Nothing ->
                case model.httpError of
                    Just httpError ->
                        div
                            [ class "alert alert-danger"
                            , role "alert"
                            ]
                            [ strong []
                                [ text <|
                                    I18n.translate language I18n.SameKeyPropertiesRetrievalFailed
                                        ++ I18n.translate language I18n.Colon
                                ]
                            , text <| Http.Error.toString language httpError
                            ]

                    Nothing ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]
