module Arguments.Index.View exposing (..)

import Arguments.Index.Types exposing (..)
import Arguments.New.View
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Urls
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock)
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
        case model.debatePropertyIds of
            Just debatePropertyIds ->
                div []
                    [ h1 [ class "d-flex" ]
                        [ span [ class "mr-3" ] [ text <| I18n.translate language I18n.ArgumentsAbout ]
                        , em []
                            [ aForPath
                                (ForParent << Navigate)
                                language
                                (Urls.objectIdPath model.objectId data)
                                []
                                [ viewValueIdLine
                                    language
                                    Nothing
                                    data
                                    False
                                    model.objectId
                                ]
                            ]
                        ]
                    , hr [] []
                    , viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
                    , hr [] []
                    , Arguments.New.View.view model.newArgumentModel
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
