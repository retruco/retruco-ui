module LineViews exposing (..)

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
            BooleanValue _ ->
                I18n.Boolean

            IdArrayValue _ ->
                I18n.IdArray

            IdValue _ ->
                I18n.Id

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
            I18n.getName language data card
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
                div [ class "ml-4" ]
                    [ viewStatementIdLine
                        language
                        navigateMsg
                        True
                        False
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
                [ viewStatementIdLine
                    language
                    navigateMsg
                    True
                    False
                    data
                    property.valueId
                ]
            ]


viewStatementIdLine : I18n.Language -> Maybe (String -> msg) -> Bool -> Bool -> DataProxy a -> String -> Html msg
viewStatementIdLine language navigateMsg independent showDetails data statementId =
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
                            viewValueTypeLine language navigateMsg showDetails data typedValue.value

                        Nothing ->
                            i [ class "text-warning" ] [ text ("Missing statement with ID: " ++ statementId) ]


viewValueIdLine : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> String -> Html msg
viewValueIdLine language navigateMsg showDetails data valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueTypeLine language navigateMsg showDetails data typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueTypeLine : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> ValueType -> Html msg
viewValueTypeLine language navigateMsg showDetails data valueType =
    if showDetails then
        div []
            [ i [] [ text (valueTypeToTypeLabel language valueType) ]
            , text (I18n.translate language I18n.Colon)
            , viewValueTypeLineContent language navigateMsg showDetails data valueType
            ]
    else
        viewValueTypeLineContent language navigateMsg showDetails data valueType


viewValueTypeLineContent : I18n.Language -> Maybe (String -> msg) -> Bool -> DataProxy a -> ValueType -> Html msg
viewValueTypeLineContent language navigateMsg showDetails data valueType =
    case valueType of
        BooleanValue bool ->
            text (toString bool)

        EmailValue str ->
            aIfIsUrl [] str

        IdArrayValue ids ->
            ul []
                (List.map
                    (\id ->
                        li
                            []
                            [ viewStatementIdLine language navigateMsg True showDetails data id ]
                    )
                    ids
                )

        IdValue id ->
            viewStatementIdLine language navigateMsg True showDetails data id

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

        WrongValue str schemaId ->
            div []
                [ p [ style [ ( "color", "red" ) ] ] [ text "Wrong value!" ]
                , pre [] [ text str ]
                , p [] [ text ("schemaId: " ++ schemaId) ]
                ]
