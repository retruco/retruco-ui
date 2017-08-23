module Values.Item.View exposing (..)

import Arguments.Index.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import LineViews exposing (viewValueTypeLine)
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock)
import Values.Item.Types exposing (..)
import Views


view : Model -> Html Msg
view model =
    case model.argumentsModel of
        Just argumentsModel ->
            Arguments.Index.View.view argumentsModel
                |> Html.map translateArgumentsMsg

        Nothing ->
            let
                data =
                    model.data

                language =
                    model.language
            in
                case ( Dict.get model.id data.values, model.debatePropertyIds ) of
                    ( Just typedValue, Just debatePropertyIds ) ->
                        div []
                            [ viewValueTypeLine
                                language
                                (Just (ForParent << Navigate))
                                True
                                data
                                typedValue.value
                            , hr [] []
                            , viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
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
                                            I18n.translate language I18n.ValueRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
