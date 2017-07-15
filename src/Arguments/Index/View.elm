module Arguments.Index.View exposing (..)

import Arguments.Index.Types exposing (..)
import Arguments.New.View
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Urls
import Values.ViewsHelpers exposing (viewValueIdLine)
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
        case model.propertyIds of
            Just propertyIds ->
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
                    , if List.isEmpty propertyIds then
                        p [] [ text <| I18n.translate language I18n.MissingArguments ]
                      else
                        ul [ class "list-group" ]
                            (List.filterMap
                                (\propertyId ->
                                    case Dict.get propertyId data.properties of
                                        Just property ->
                                            let
                                                ballot =
                                                    Dict.get property.ballotId data.ballots

                                                ballotRating =
                                                    Maybe.map .rating ballot

                                                keyLabel =
                                                    Dict.get property.keyId (Dict.fromList keyIdLabelCouples)
                                                        |> Maybe.map (I18n.translate language)
                                                        |> Maybe.withDefault property.keyId
                                            in
                                                Just <|
                                                    li [ class "flex-nowrap justify-content-between list-group-item" ]
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
                                                                [ h4 [] [ text keyLabel ]
                                                                , viewValueIdLine
                                                                    language
                                                                    (Just (ForParent << Navigate))
                                                                    data
                                                                    False
                                                                    property.valueId
                                                                ]
                                                            ]
                                                        , div []
                                                            [ aForPath
                                                                (ForParent << Navigate)
                                                                language
                                                                ("/values/" ++ property.valueId ++ "/arguments")
                                                                [ class "btn btn-secondary" ]
                                                                [ text (I18n.translate language (I18n.Debate)) ]
                                                            , div
                                                                [ class "btn-group-vertical"
                                                                , role "group"
                                                                , ariaLabel "Rating panel"
                                                                ]
                                                                [ button
                                                                    [ ariaPressed (ballotRating == Just 1)
                                                                    , classList
                                                                        [ ( "active", ballotRating == Just 1 )
                                                                        , ( "btn", True )
                                                                        , ( "btn-secondary", True )
                                                                        ]
                                                                    , onClick
                                                                        (ForSelf
                                                                            (if ballotRating == Just 1 then
                                                                                UnvoteRating property.id
                                                                             else
                                                                                VoteRatingUp property.id
                                                                            )
                                                                        )
                                                                    , type_ "button"
                                                                    ]
                                                                    [ span
                                                                        [ ariaHidden True
                                                                        , class "fa fa-thumbs-o-up"
                                                                        ]
                                                                        []
                                                                    , span
                                                                        [ class "sr-only" ]
                                                                        [ text <|
                                                                            I18n.translate
                                                                                language
                                                                                I18n.GivePositiveRating
                                                                        ]
                                                                    ]
                                                                , button
                                                                    [ class "btn btn-secondary"
                                                                    , disabled True
                                                                    , type_ "button"
                                                                    ]
                                                                    [ text
                                                                        ((toString property.ratingSum)
                                                                            ++ " / "
                                                                            ++ (toString property.ratingCount)
                                                                        )
                                                                    ]
                                                                , button
                                                                    [ ariaPressed (ballotRating == Just -1)
                                                                    , classList
                                                                        [ ( "active", ballotRating == Just -1 )
                                                                        , ( "btn", True )
                                                                        , ( "btn-secondary", True )
                                                                        ]
                                                                    , onClick
                                                                        (ForSelf
                                                                            (if ballotRating == Just -1 then
                                                                                UnvoteRating property.id
                                                                             else
                                                                                VoteRatingDown property.id
                                                                            )
                                                                        )
                                                                    , type_ "button"
                                                                    ]
                                                                    [ span
                                                                        [ ariaHidden True
                                                                        , class "fa fa-thumbs-o-down"
                                                                        ]
                                                                        []
                                                                    , span
                                                                        [ class "sr-only" ]
                                                                        [ text <|
                                                                            I18n.translate
                                                                                language
                                                                                I18n.GiveNegativeRating
                                                                        ]
                                                                    ]
                                                                ]
                                                            ]
                                                        ]

                                        Nothing ->
                                            Nothing
                                )
                                propertyIds
                            )
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
