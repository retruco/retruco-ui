module Statements.Toolbar.View exposing (..)

import Configuration
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick, onWithOptions)
import Http
import I18n
import Images
import Json.Decode
import Statements.Toolbar.Types exposing (..)
import Strings
import Types exposing (..)
import Urls


view : Model (Statement b) -> Html Msg
view model =
    div [ class "align-items-start d-flex justify-content-between" ]
        [ viewStatementRatingToolbar
            model.language
            model.data
            model.statement.ballotId
            model.trashPropertyId
        , viewStatementSocialToolbar
            model.embed
            model.language
            (ForSelf << ShareOnFacebook)
            (ForSelf << ShareOnGooglePlus)
            (ForSelf << ShareOnLinkedIn)
            (ForSelf << ShareOnTwitter)
            model.data
            model.statement
        ]


viewStatementRatingToolbar : I18n.Language -> DataProxy a -> String -> Maybe String -> Html Msg
viewStatementRatingToolbar language data ballotId trashPropertyId =
    let
        ballot =
            Dict.get ballotId data.ballots
                |> Maybe.andThen
                    (\ballot ->
                        if ballot.deleted then
                            Nothing
                        else
                            Just ballot
                    )

        ballotRating =
            Maybe.map .rating ballot

        rateMsg =
            (ForSelf << Rate)

        trashBallotRating =
            trashPropertyId
                |> Maybe.andThen (\trashPropertyId -> Dict.get trashPropertyId data.properties)
                |> Maybe.map .ballotId
                |> Maybe.andThen (\trashBallotId -> Dict.get trashBallotId data.ballots)
                |> Maybe.andThen
                    (\trashBallot ->
                        if trashBallot.deleted then
                            Nothing
                        else
                            Just trashBallot
                    )
                |> Maybe.map .rating

        trashMsg =
            (ForSelf << Trash)
    in
        div
            [ class "d-flex toolbar"
            , role "toolbar"
            ]
            [ let
                pressed =
                    ballotRating == Nothing
              in
                button
                    ([ ariaDisabled pressed
                     , class <|
                        if pressed then
                            "btn btn-secondary"
                        else
                            "btn btn-outline-secondary"
                     , disabled pressed
                     , type_ "button"
                     ]
                        ++ if pressed then
                            []
                           else
                            [ onClick <| rateMsg Nothing ]
                    )
                    [ span
                        [ ariaHidden True
                        , class "fa fa-close"
                        ]
                        []
                    , text " "
                    , text <| I18n.translate language I18n.AbstainAction
                    ]
            , div
                [ ariaLabel "Rating panel"
                , class "btn-group ml-3"
                , role "group"
                ]
                [ let
                    pressed =
                        ballotRating == Just 1
                  in
                    button
                        [ ariaDisabled pressed
                        , class <|
                            if pressed then
                                "btn btn-success"
                            else
                                "btn btn-outline-success"
                        , disabled pressed
                        , onClick
                            (if pressed then
                                rateMsg Nothing
                             else
                                rateMsg (Just 1)
                            )
                        , type_ "button"
                        ]
                        [ span
                            [ ariaHidden True
                            , class "fa fa-thumbs-o-up"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VotePlusAction
                        ]
                , let
                    pressed =
                        ballotRating == Just 0
                  in
                    button
                        [ ariaDisabled pressed
                        , class <|
                            if pressed then
                                "btn btn-secondary"
                            else
                                "btn btn-outline-secondary"
                        , disabled pressed
                        , onClick
                            (if pressed then
                                rateMsg Nothing
                             else
                                rateMsg (Just 0)
                            )
                        , type_ "button"
                        ]
                        [ span
                            [ ariaHidden True
                            , class "fa fa-square-o"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VoteNeutralAction
                        ]
                , let
                    pressed =
                        ballotRating == Just -1
                  in
                    button
                        [ ariaDisabled pressed
                        , class <|
                            if pressed then
                                "btn btn-warning"
                            else
                                "btn btn-outline-warning"
                        , disabled pressed
                        , onClick
                            (if pressed then
                                rateMsg Nothing
                             else
                                rateMsg (Just -1)
                            )
                        , type_ "button"
                        ]
                        [ span
                            [ ariaHidden True
                            , class "fa fa-thumbs-o-down"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VoteMinusAction
                        ]
                ]
            , div [ class "dropdown ml-3" ]
                [ button
                    [ attribute "aria-expanded" "false"
                    , attribute "aria-haspopup" "true"
                    , class "btn btn-outline-danger dropdown-toggle"
                    , attribute "data-toggle" "dropdown"
                    , type_ "button"
                    ]
                    [ span
                        [ ariaHidden True
                        , class "fa fa-trash-o"
                        ]
                        []
                    , text " "
                    , text <|
                        I18n.translate
                            language
                            I18n.Trash
                    , span
                        [ classList
                            [ ( "badge", True )
                            , ( case trashBallotRating of
                                    Just 1 ->
                                        "badge-warning"

                                    Just -1 ->
                                        "badge-success"

                                    _ ->
                                        "badge-secondary"
                              , True
                              )
                            , ( "badge-pill", True )
                            , ( "ml-2", True )
                            ]
                        ]
                        [ span
                            [ ariaHidden True
                            , classList
                                [ ( "fa", True )
                                , ( case trashBallotRating of
                                        Just 1 ->
                                            "fa-thumbs-o-up"

                                        Just 0 ->
                                            "fa-square-o"

                                        Just -1 ->
                                            "fa-thumbs-o-down"

                                        _ ->
                                            "fa-close"
                                  , True
                                  )
                                ]
                            ]
                            []
                        ]
                    , text " "
                    ]
                , div [ class "dropdown-menu" ]
                    [ h5 [ class "dropdown-header" ] [ text <| I18n.translate language I18n.TrashThisItemQuestion ]
                    , button
                        ([ classList
                            [ ( "dropdown-item", True )
                            , ( "disabled", trashBallotRating == Nothing )
                            ]
                         , type_ "button"
                         ]
                            ++ if trashBallotRating == Nothing then
                                []
                               else
                                [ onClick <| trashMsg <| RateTrash Nothing ]
                        )
                        [ i
                            [ ariaHidden True
                            , class
                                (if trashBallotRating == Nothing then
                                    "fa fa-circle fa-fw mr-3"
                                 else
                                    "fa fa-fw fa-circle-o mr-3"
                                )
                            ]
                            []
                        , text " "
                        , span
                            [ ariaHidden True
                            , class "fa fa-close"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.AbstainAction
                        ]
                    , div [ class "dropdown-divider" ] []
                    , button
                        [ class "dropdown-item"
                        , onClick <|
                            trashMsg <|
                                RateTrash <|
                                    (if trashBallotRating == Just 1 then
                                        Nothing
                                     else
                                        Just 1
                                    )
                        , type_ "button"
                        ]
                        [ i
                            [ ariaHidden True
                            , class
                                (if trashBallotRating == Just 1 then
                                    "fa fa-circle fa-fw mr-3"
                                 else
                                    "fa fa-fw fa-circle-o mr-3"
                                )
                            ]
                            []
                        , text " "
                        , span
                            [ ariaHidden True
                            , class "fa fa-thumbs-o-up"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VotePlusAction
                        ]
                    , button
                        [ class "dropdown-item"
                        , onClick <|
                            trashMsg <|
                                RateTrash <|
                                    (if trashBallotRating == Just 0 then
                                        Nothing
                                     else
                                        Just 0
                                    )
                        , type_ "button"
                        ]
                        [ i
                            [ ariaHidden True
                            , class
                                (if trashBallotRating == Just 0 then
                                    "fa fa-circle fa-fw mr-3"
                                 else
                                    "fa fa-fw fa-circle-o mr-3"
                                )
                            ]
                            []
                        , text " "
                        , span
                            [ ariaHidden True
                            , class "fa fa-square-o"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VoteNeutralAction
                        ]
                    , button
                        [ class "dropdown-item"
                        , onClick <|
                            trashMsg <|
                                RateTrash <|
                                    (if trashBallotRating == Just -1 then
                                        Nothing
                                     else
                                        Just -1
                                    )
                        , type_ "button"
                        ]
                        [ i
                            [ ariaHidden True
                            , class
                                (if trashBallotRating == Just -1 then
                                    "fa fa-circle fa-fw mr-3"
                                 else
                                    "fa fa-fw fa-circle-o mr-3"
                                )
                            ]
                            []
                        , text " "
                        , span
                            [ ariaHidden True
                            , class "fa fa-thumbs-o-down"
                            ]
                            []
                        , text " "
                        , text <| I18n.translate language I18n.VoteMinusAction
                        ]
                    , div [ class "dropdown-divider" ] []
                    , button
                        [ class "dropdown-item"
                        , onClick <| trashMsg DebateTrash
                        , type_ "button"
                        ]
                        [ text <| I18n.translate language I18n.DebateTrashingAction ]
                    ]
                ]
            ]


viewStatementSocialToolbar :
    Bool
    -> I18n.Language
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> DataProxy a
    -> { b | id : String }
    -> Html msg
viewStatementSocialToolbar embed language shareOnFacebookMsg shareOnGooglePlusMsg shareOnLinkedInMsg shareOnTwitterMsg data { id } =
    div
        [ class "toolbar"
        , role "toolbar"
        ]
        [ let
            statementString =
                Strings.idToString language data id

            imageUrl =
                Images.idToImageUrl language data id

            url =
                Urls.idToPath data id
                    |> Urls.languagePath embed language
                    |> Urls.fullUrl

            facebookUrl =
                "http://www.facebook.com/sharer.php?s=100&p[title]="
                    ++ Http.encodeUri statementString
                    ++ "&p[summary]="
                    ++ Http.encodeUri (I18n.translate language (I18n.TweetMessage statementString url))
                    ++ "&p[url]="
                    ++ Http.encodeUri url
                    ++ "&p[images][0]="
                    ++ Http.encodeUri imageUrl

            googlePlusUrl =
                "https://plus.google.com/share?url=" ++ Http.encodeUri url

            linkedInUrl =
                "https://www.linkedin.com/shareArticle?mini=true&url="
                    ++ Http.encodeUri url
                    ++ "&title="
                    ++ Http.encodeUri statementString
                    ++ "&summary="
                    ++ Http.encodeUri (I18n.translate language (I18n.TweetMessage statementString url))
                    ++ "&source="
                    ++ Http.encodeUri Configuration.appTitle

            twitterUrl =
                "https://twitter.com/intent/tweet?text="
                    ++ Http.encodeUri (I18n.translate language (I18n.TweetMessage statementString url))
          in
            div []
                [ a
                    [ class "btn btn-light"
                    , href facebookUrl
                    , onWithOptions
                        "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (shareOnFacebookMsg facebookUrl))
                    ]
                    [ i [ ariaHidden True, class "fa fa-facebook" ] [] ]
                , a
                    [ class "btn btn-light"
                    , href googlePlusUrl
                    , onWithOptions
                        "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (shareOnGooglePlusMsg googlePlusUrl))
                    ]
                    [ i [ ariaHidden True, class "fa fa-google-plus" ] [] ]
                , a
                    [ class "btn btn-light"
                    , href linkedInUrl
                    , onWithOptions
                        "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (shareOnLinkedInMsg linkedInUrl))
                    ]
                    [ i [ ariaHidden True, class "fa fa-linkedin" ] [] ]
                , a
                    [ class "btn btn-light"
                    , href twitterUrl
                    , onWithOptions
                        "click"
                        { stopPropagation = True, preventDefault = True }
                        (Json.Decode.succeed (shareOnTwitterMsg twitterUrl))
                    ]
                    [ i [ ariaHidden True, class "fa fa-twitter" ] [] ]
                ]
        ]
