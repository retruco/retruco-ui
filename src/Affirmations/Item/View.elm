module Affirmations.Item.View exposing (..)

import Affirmations.Item.Types exposing (..)
import Arguments.New.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import LineViews exposing (viewValueIdLine, viewValueTypeLine)
import Statements.ViewsHelpers
    exposing
        ( viewDebatePropertiesBlock
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
        case ( Dict.get model.id data.values, model.debatePropertyIds ) of
            ( Just typedValue, Just debatePropertyIds ) ->
                div []
                    [ div [ class "align-items-center d-flex flex-nowrap justify-content-between" ]
                        [ h1 []
                            [ viewValueTypeLine
                                language
                                (Just (ForParent << Navigate))
                                data
                                False
                                typedValue.value
                            ]
                        , viewStatementRatingPanel language (ForParent << Navigate) Nothing typedValue
                        ]
                    , viewStatementRatingToolbar
                        language
                        data
                        (\id rating -> ForSelf <| Rate id rating)
                        (ForSelf << Trash)
                        typedValue
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
