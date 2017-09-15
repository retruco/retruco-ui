module Properties.SameValue.View exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Http.Error
import I18n
import Properties.SameValue.Types exposing (..)
import Statements.Lines exposing (lineIdAttributes, viewPropertyIdLine)
import Statements.RatingPanels exposing (viewStatementIdRatingPanel)
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        language =
            model.language

        navigateMsg =
            (ForParent << Navigate)
    in
        case model.propertyIds of
            Just propertyIds ->
                div []
                    [ div []
                        [ if Array.isEmpty propertyIds then
                            p [] [ text <| I18n.translate language I18n.MissingArguments ]
                          else
                            ul [ class "list-group" ]
                                (Array.toList propertyIds
                                    |> List.map
                                        (\propertyId ->
                                            li
                                                (lineIdAttributes
                                                    language
                                                    (Just navigateMsg)
                                                    [ ( "list-group-item", True ) ]
                                                    data
                                                    propertyId
                                                )
                                                [ viewPropertyIdLine
                                                    language
                                                    True
                                                    data
                                                    propertyId
                                                , viewStatementIdRatingPanel language data propertyId
                                                ]
                                        )
                                )
                        ]
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
