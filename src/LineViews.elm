module LineViews exposing (..)

import Constants exposing (nameKeyIds)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Types exposing (..)
import Urls


keyIdLabelCouples : List ( String, I18n.TranslationId )
keyIdLabelCouples =
    [ ( "pros", I18n.DebateArgumentFor )
    , ( "cons", I18n.DebateArgumentAgainst )
    ]


valueTypeToTypeLabel : I18n.Language -> ValueType -> String
valueTypeToTypeLabel language valueType =
    I18n.translate language <|
        case valueType of
            BijectiveCardReferenceValue _ ->
                I18n.BijectiveCardReference

            BooleanValue _ ->
                I18n.Boolean

            CardIdArrayValue _ ->
                I18n.CardIdArray

            CardIdValue _ ->
                I18n.CardId

            EmailValue _ ->
                I18n.Email

            ImagePathValue _ ->
                I18n.Image

            LocalizedStringValue _ ->
                I18n.LocalizedString

            NumberValue _ ->
                I18n.Number

            StringValue _ ->
                I18n.String

            UrlValue _ ->
                I18n.Url

            ValueIdArrayValue _ ->
                I18n.ValueIdArray

            ValueIdValue _ ->
                I18n.ValueId

            WrongValue _ schemaId ->
                I18n.UnknownSchemaId schemaId


viewCardIdLine : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> String -> Html msg
viewCardIdLine language navigateMsg data cardId =
    case Dict.get cardId data.cards of
        Just card ->
            viewCardLine language navigateMsg data card

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing card with ID: " ++ cardId) ]


viewCardLine : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> Card -> Html msg
viewCardLine language navigateMsg data card =
    let
        cardName =
            I18n.getOneString language nameKeyIds card data.values
                |> Maybe.withDefault card.id
    in
        case navigateMsg of
            Just navigateMsg ->
                aForPath navigateMsg language ("/cards/" ++ card.id) [] [ text cardName ]

            Nothing ->
                text cardName


viewPropertyIdLine : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> String -> Html msg
viewPropertyIdLine language navigateMsg independent data propertyId =
    case Dict.get propertyId data.properties of
        Just property ->
            viewPropertyLine language navigateMsg independent data property

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing property with ID: " ++ propertyId) ]


viewPropertyLine : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> Property -> Html msg
viewPropertyLine language navigateMsg independent data property =
    -- The `independent` flag indicates whether to display the object of the property along with it key and value.
    let
        keyLabel =
            Dict.get property.keyId (Dict.fromList keyIdLabelCouples)
                |> Maybe.map (I18n.translate language)
                |> Maybe.withDefault property.keyId
    in
        div []
            [ if independent then
                div []
                    [ viewStatementIdLine
                        language
                        navigateMsg
                        True
                        data
                        property.objectId
                    ]
              else
                text ""
            , div [ class "align-items-baseline d-flex flex-nowrap" ]
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
                        , ( "fa-fw", True )
                        , ( "mr-2", True )
                        ]
                    ]
                    []
                , span [] [ text keyLabel ]
                ]
            , div []
                [ viewStatementIdLine
                    language
                    navigateMsg
                    True
                    data
                    property.valueId
                ]
            ]


viewStatementIdLine : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> String -> Html msg
viewStatementIdLine language navigateMsg independent data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewCardLine language navigateMsg data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewPropertyLine language navigateMsg independent data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewValueTypeLine language navigateMsg data False typedValue.value

                        Nothing ->
                            i [ class "text-warning" ] [ text ("Missing statement with ID: " ++ statementId) ]


viewValueIdLine : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> Bool -> String -> Html msg
viewValueIdLine language navigateMsg data showDetails valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueTypeLine language navigateMsg data showDetails typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueTypeLine : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> Bool -> ValueType -> Html msg
viewValueTypeLine language navigateMsg data showDetails valueType =
    if showDetails then
        div []
            [ i [] [ text (valueTypeToTypeLabel language valueType) ]
            , text (I18n.translate language I18n.Colon)
            , viewValueTypeLineContent language navigateMsg data showDetails valueType
            ]
    else
        viewValueTypeLineContent language navigateMsg data showDetails valueType


viewValueTypeLineContent : I18n.Language -> Maybe (String -> msg) -> DataProxy a -> Bool -> ValueType -> Html msg
viewValueTypeLineContent language navigateMsg data showDetails valueType =
    case valueType of
        BijectiveCardReferenceValue { targetId } ->
            viewCardIdLine language navigateMsg data targetId

        BooleanValue bool ->
            text (toString bool)

        CardIdArrayValue childValues ->
            ul []
                (List.map
                    (\childValue ->
                        li
                            []
                            [ viewValueTypeLine language navigateMsg data showDetails (CardIdValue childValue) ]
                    )
                    childValues
                )

        CardIdValue cardId ->
            viewCardIdLine language navigateMsg data cardId

        EmailValue str ->
            aIfIsUrl [] str

        ImagePathValue path ->
            figure
                [ class "figure text-center" ]
                [ img
                    [ alt <| I18n.translate language I18n.ImageAlt
                    , class "figure-img img-fluid rounded"
                    , src (Urls.fullApiUrl path ++ "?dim=96")
                    , style [ ( "max-width", "96px" ) ]
                    ]
                    []
                , figcaption [ class "figure-caption" ] [ text path ]
                ]

        LocalizedStringValue values ->
            if showDetails || Dict.size values > 1 then
                dl []
                    (values
                        |> Dict.toList
                        |> List.concatMap
                            (\( languageCode, childValue ) ->
                                [ dt [] [ text languageCode ]
                                , dd [] [ aIfIsUrl [] childValue ]
                                ]
                            )
                    )
            else
                div []
                    (values
                        |> Dict.toList
                        |> List.map (\( languageCode, childValue ) -> aIfIsUrl [] childValue)
                    )

        NumberValue float ->
            text (toString float)

        StringValue str ->
            aIfIsUrl [] str

        UrlValue str ->
            aIfIsUrl [] str

        ValueIdArrayValue childValues ->
            ul []
                (List.map
                    (\childValue ->
                        li
                            []
                            [ viewValueTypeLine language navigateMsg data showDetails (ValueIdValue childValue) ]
                    )
                    childValues
                )

        ValueIdValue valueId ->
            case Dict.get valueId data.values of
                Just subValue ->
                    viewValueTypeLine language navigateMsg data showDetails subValue.value

                Nothing ->
                    text ("Error: referenced value not found for valueId: " ++ valueId)

        WrongValue str schemaId ->
            div []
                [ p [ style [ ( "color", "red" ) ] ] [ text "Wrong value!" ]
                , pre [] [ text str ]
                , p [] [ text ("schemaId: " ++ schemaId) ]
                ]