module Statements.ViewsHelpers exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import LineViews exposing (viewPropertyIdLine, viewStatementIdLine)
import Types exposing (Argument, DataProxy, Statement)
import Urls


viewDuplicatedByAlert : I18n.Language -> (String -> msg) -> DataProxy a -> Maybe (List String) -> Html msg
viewDuplicatedByAlert language navigateMsg data duplicatedByPropertyIds =
    case duplicatedByPropertyIds of
        Just duplicatedByPropertyIds ->
            let
                duplicatedByProperties =
                    List.filterMap
                        (\duplicatedByPropertyId -> Dict.get duplicatedByPropertyId data.properties)
                        duplicatedByPropertyIds

                duplicatedByPropertyCount =
                    List.length duplicatedByProperties
            in
                if duplicatedByPropertyCount > 0 then
                    div [ class "alert alert-secondary", role "alert" ]
                        [ h3 [ class "alert-heading" ]
                            [ text <|
                                I18n.translate language <|
                                    I18n.DuplicatedByTitle duplicatedByPropertyCount
                            ]
                        , p []
                            [ text <|
                                I18n.translate language <|
                                    I18n.DuplicatedByDescription duplicatedByPropertyCount
                            ]
                        , ul [ class "list-group" ]
                            (List.map
                                (\duplicatedByProperty ->
                                    li [ class "d-flex flex-nowrap justify-content-between list-group-item" ]
                                        [ viewStatementIdLine
                                            language
                                            (Just navigateMsg)
                                            True
                                            False
                                            data
                                            duplicatedByProperty.objectId
                                        , viewStatementIdRatingPanel
                                            language
                                            navigateMsg
                                            data
                                            duplicatedByProperty.objectId
                                        ]
                                )
                                duplicatedByProperties
                            )
                        ]
                else
                    text ""

        Nothing ->
            text ""


viewDuplicateOfAlert : I18n.Language -> (String -> msg) -> DataProxy a -> Maybe (List String) -> Html msg
viewDuplicateOfAlert language navigateMsg data duplicateOfPropertyIds =
    case duplicateOfPropertyIds of
        Just duplicateOfPropertyIds ->
            let
                duplicateOfProperties =
                    List.filterMap
                        (\duplicateOfPropertyId -> Dict.get duplicateOfPropertyId data.properties)
                        duplicateOfPropertyIds

                duplicateOfPropertyCount =
                    List.length duplicateOfProperties
            in
                if duplicateOfPropertyCount > 0 then
                    div [ class "alert alert-warning", role "alert" ]
                        [ h3 [ class "alert-heading" ]
                            [ text <|
                                I18n.translate language <|
                                    I18n.DuplicateOfTitle duplicateOfPropertyCount
                            ]
                        , p []
                            [ text <|
                                I18n.translate language <|
                                    I18n.DuplicateOfDescription duplicateOfPropertyCount
                            ]
                        , ul [ class "list-group" ]
                            (List.map
                                (\duplicateOfProperty ->
                                    li [ class "d-flex flex-nowrap justify-content-between list-group-item" ]
                                        [ viewStatementIdLine
                                            language
                                            (Just navigateMsg)
                                            True
                                            False
                                            data
                                            duplicateOfProperty.valueId
                                        , viewStatementIdRatingPanel
                                            language
                                            navigateMsg
                                            data
                                            duplicateOfProperty.valueId
                                        ]
                                )
                                duplicateOfProperties
                            )
                        ]
                else
                    text ""

        Nothing ->
            text ""


viewStatementIdRatingPanel : I18n.Language -> (String -> msg) -> DataProxy a -> String -> Html msg
viewStatementIdRatingPanel language navigateMsg data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewStatementRatingPanel language navigateMsg True data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewStatementRatingPanel language navigateMsg True data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewStatementRatingPanel language navigateMsg True data typedValue

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewStatementRatingPanel :
    I18n.Language
    -> (String -> msg)
    -> Bool
    -> DataProxy a
    -> { b | argumentCount : Int, id : String, ratingCount : Int, ratingSum : Int, trashed : Bool }
    -> Html msg
viewStatementRatingPanel language navigateMsg addLink data { argumentCount, id, ratingCount, ratingSum, trashed } =
    let
        buttonClass =
            classList
                [ ( "btn", True )
                , ( "btn-lg", True )
                , ( if trashed then
                        "btn-danger"
                    else if ratingSum > 0 then
                        "btn-outline-success"
                    else
                        "btn-outline-danger"
                  , True
                  )
                , ( "ml-3", True )
                ]

        buttonWithAttributes =
            if addLink then
                aForPath
                    navigateMsg
                    language
                    (Urls.idToPath data id)
                    [ buttonClass ]
            else
                button
                    [ buttonClass
                    , disabled True
                    , type_ "button"
                    ]
    in
        buttonWithAttributes
            [ strong [] [ text <| toString ratingSum ]
            , text " / "
            , text <|
                I18n.translate
                    language
                    (I18n.CountVotes ratingCount)
            , br [] []
            , text <|
                I18n.translate
                    language
                    (I18n.CountArguments argumentCount)
            ]
