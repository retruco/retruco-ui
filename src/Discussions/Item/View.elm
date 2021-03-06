module Discussions.Item.View exposing (..)

import Discussions.Item.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Ideas.Index.View
import Interventions.Index.View
import Questions.Index.View
import Urls
import Views


view : Model -> Html Msg
view model =
    let
        data =
            model.data

        embed =
            model.embed

        language =
            model.language

        navigateMsg =
            ForParent << Navigate
    in
        div []
            [ ul [ class "nav nav-tabs" ]
                [ li [ class "nav-item" ]
                    [ aForPath
                        navigateMsg
                        embed
                        language
                        (Urls.idToInterventionsPath data model.objectId)
                        [ classList
                            [ ( "active"
                              , case model.activeTab of
                                    InterventionsTab _ ->
                                        True

                                    _ ->
                                        False
                              )
                            , ( "nav-link", True )
                            ]
                        ]
                        [ text <| I18n.translate language I18n.Interventions ]
                    ]
                , li [ class "nav-item" ]
                    [ aForPath
                        navigateMsg
                        embed
                        language
                        (Urls.idToIdeasPath data model.objectId)
                        [ classList
                            [ ( "active"
                              , case model.activeTab of
                                    IdeasTab _ ->
                                        True

                                    _ ->
                                        False
                              )
                            , ( "nav-link", True )
                            ]
                        ]
                        [ text <| I18n.translate language I18n.Ideas ]
                    ]
                , li [ class "nav-item" ]
                    [ aForPath
                        navigateMsg
                        embed
                        language
                        (Urls.idToQuestionsPath data model.objectId)
                        [ classList
                            [ ( "active"
                              , case model.activeTab of
                                    QuestionsTab _ ->
                                        True

                                    _ ->
                                        False
                              )
                            , ( "nav-link", True )
                            ]
                        ]
                        [ text <| I18n.translate language I18n.Questions ]
                    ]
                ]
            , case model.discussionProperties of
                Just _ ->
                    case model.activeTab of
                        IdeasTab ideasModel ->
                            Ideas.Index.View.view ideasModel
                                |> Html.map translateIdeasMsg

                        InterventionsTab interventionsModel ->
                            Interventions.Index.View.view interventionsModel
                                |> Html.map translateInterventionsMsg

                        NoTab ->
                            text ""

                        QuestionsTab questionsModel ->
                            Questions.Index.View.view questionsModel
                                |> Html.map translateQuestionsMsg

                Nothing ->
                    case model.httpError of
                        Just httpError ->
                            div
                                [ class "alert alert-danger"
                                , role "alert"
                                ]
                                [ strong []
                                    [ text <|
                                        I18n.translate language I18n.InterventionsRetrievalFailed
                                            ++ I18n.translate language I18n.Colon
                                    ]
                                , text <| Http.Error.toString language httpError
                                ]

                        Nothing ->
                            div [ class "text-center" ]
                                [ Views.viewLoading language ]
            ]
