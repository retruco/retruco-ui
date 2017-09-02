module DebateProperties.SameObject.View exposing (..)

import DebateProperties.New.View
import DebateProperties.SameObject.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import LineViews exposing (viewValueIdLine)
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock)
import Urls
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language
    in
        case model.debatePropertyIds of
            Just debatePropertyIds ->
                div []
                    [ viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
                    , hr [] []
                    , DebateProperties.New.View.view model.newDebatePropertyModel
                        |> Html.map translateNewArgumentMsg
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
                                    I18n.translate language I18n.ArgumentsRetrievalFailed
                                        ++ I18n.translate language I18n.Colon
                                ]
                            , text <| Http.Error.toString language httpError
                            ]

                    Nothing ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]