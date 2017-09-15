module Statements.Alerts exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import I18n
import Statements.Lines exposing (lineIdAttributes, viewStatementIdLine)
import Statements.RatingPanels exposing (viewStatementIdRatingPanel)
import Types exposing (Argument, DataProxy, Statement)


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
                                    li
                                        (lineIdAttributes
                                            language
                                            (Just navigateMsg)
                                            [ ( "list-group-item", True ) ]
                                            data
                                            duplicatedByProperty.objectId
                                        )
                                        [ viewStatementIdLine
                                            language
                                            True
                                            False
                                            data
                                            duplicatedByProperty.objectId
                                        , viewStatementIdRatingPanel language data duplicatedByProperty.objectId
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
                                    li
                                        (lineIdAttributes
                                            language
                                            (Just navigateMsg)
                                            [ ( "list-group-item", True ) ]
                                            data
                                            duplicateOfProperty.valueId
                                        )
                                        [ viewStatementIdLine
                                            language
                                            True
                                            False
                                            data
                                            duplicateOfProperty.valueId
                                        , viewStatementIdRatingPanel language data duplicateOfProperty.valueId
                                        ]
                                )
                                duplicateOfProperties
                            )
                        ]
                else
                    text ""

        Nothing ->
            text ""
