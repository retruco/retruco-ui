module Properties.SameObject.View exposing (..)

import Array
import Constants exposing (debateKeyIds)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Properties.KeysAutocomplete.View
import Properties.SameObject.Types exposing (..)
import Set exposing (Set)
import Statements.Lines exposing (viewStatementIdLine)
import Strings
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
            (ForParent << Navigate)
    in
        case model.propertyIds of
            Just propertyIds ->
                let
                    properties =
                        Array.toList propertyIds
                            |> List.filterMap (\propertyId -> Dict.get propertyId data.properties)

                    debateKeyIdLabelCouples =
                        properties
                            |> List.map .keyId
                            |> Set.fromList
                            |> Set.toList
                            |> List.filter (\keyId -> not <| List.member keyId debateKeyIds)
                            |> List.map
                                (\keyId ->
                                    ( keyId
                                    , Dict.get keyId I18n.debateKeyLabelById
                                        |> Maybe.map (I18n.translate language)
                                        |> Maybe.withDefault (Strings.idToString language data keyId)
                                    )
                                )
                            |> List.sortBy (\( keyId, label ) -> label)
                in
                    div []
                        [ div []
                            [ if List.isEmpty properties then
                                p [] [ text <| I18n.translate language I18n.MissingProperties ]
                              else
                                ul [ class "list-group" ]
                                    (List.map
                                        (\( keyId, keyLabel ) ->
                                            let
                                                keyProperties =
                                                    properties
                                                        |> List.filter
                                                            (\property ->
                                                                (property.keyId == keyId) && (property.ratingSum > 0)
                                                            )
                                                        |> List.sortBy
                                                            (\property -> ( property.ratingSum, property.createdAt ))
                                                        |> List.reverse

                                                valueIds =
                                                    List.map .valueId keyProperties
                                            in
                                                li [ class "align-items-center d-flex flex-nowrap justify-content-between list-group-item" ]
                                                    [ div []
                                                        [ div [ class "align-items-baseline d-flex flex-nowrap" ]
                                                            [ span
                                                                [ ariaHidden True
                                                                , classList
                                                                    [ ( "fa", True )
                                                                    , ( if keyId == "con" then
                                                                            "fa-minus"
                                                                        else if keyId == "pro" then
                                                                            "fa-plus"
                                                                        else
                                                                            "fa-circle"
                                                                      , True
                                                                      )
                                                                    , ( "fa-fw", True )
                                                                    , ( "mr-2", True )
                                                                    ]
                                                                ]
                                                                []
                                                            , span [] [ text keyLabel ]
                                                            ]
                                                        , div [ class "ml-4" ]
                                                            [ case valueIds of
                                                                [ valueId ] ->
                                                                    viewStatementIdLine
                                                                        embed
                                                                        language
                                                                        navigateMsg
                                                                        True
                                                                        False
                                                                        data
                                                                        valueId

                                                                valueIds ->
                                                                    ul []
                                                                        (List.map
                                                                            (\valueId ->
                                                                                li []
                                                                                    [ viewStatementIdLine
                                                                                        embed
                                                                                        language
                                                                                        navigateMsg
                                                                                        True
                                                                                        False
                                                                                        data
                                                                                        valueId
                                                                                    ]
                                                                            )
                                                                            valueIds
                                                                        )
                                                            ]
                                                        ]
                                                    , div []
                                                        [ aForPath
                                                            navigateMsg
                                                            embed
                                                            language
                                                            (Urls.idToSameObjectAndKeyPropertiesPath
                                                                data
                                                                model.objectId
                                                                keyId
                                                            )
                                                            [ class "btn btn-outline-secondary" ]
                                                            [ text (I18n.translate language (I18n.Edit)) ]
                                                        ]
                                                    ]
                                        )
                                        debateKeyIdLabelCouples
                                    )
                            ]
                        , hr [] []
                        , h4 [] [ text <| I18n.translate model.language I18n.NewProperty ]
                        , let
                            controlId =
                                "keysAutocomplete"
                          in
                            Properties.KeysAutocomplete.View.viewAutocomplete
                                language
                                controlId
                                I18n.AddPropertyKey
                                I18n.PropertyKeyPlaceholder
                                Nothing
                                model.keysAutocompleteModel
                                |> Html.map translateKeysAutocompleteMsg
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
