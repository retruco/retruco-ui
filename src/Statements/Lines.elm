module Statements.Lines exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onWithOptions)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Json.Decode
import Statements.RatingPanels exposing (viewStatementIdRatingPanel)
import Strings
import Types exposing (..)
import Urls


lineIdAttributes :
    I18n.Language
    -> Maybe (String -> msg)
    -> List ( String, Bool )
    -> DataProxy a
    -> String
    -> List (Attribute msg)
lineIdAttributes language navigateMsg classItems data id =
    [ classList
        ([ ( "align", True )
         , ( "align-items-center", True )
         , ( "d-flex", True )
         , ( "flex-nowrap", True )
         , ( "justify-content-between", True )
         ]
            ++ classItems
        )
    ]
        ++ case navigateMsg of
            Just navigateMsg ->
                [ onWithOptions
                    "click"
                    { stopPropagation = True, preventDefault = False }
                    (Json.Decode.succeed (navigateMsg (Urls.languagePath language (Urls.idToPath data id))))
                , style [ ( "cursor", "pointer" ) ]
                ]

            Nothing ->
                []


valueTypeToTypeLabel : I18n.Language -> ValueType -> String
valueTypeToTypeLabel language valueType =
    I18n.translate language <|
        case valueType of
            BooleanValue _ ->
                I18n.Boolean

            IdsArrayValue _ ->
                I18n.IdsArray

            EmailValue _ ->
                I18n.Email

            ImagePathValue _ ->
                I18n.Image

            NumberValue _ ->
                I18n.Number

            StringValue _ ->
                I18n.String

            UrlValue _ ->
                I18n.Url

            WrongValue _ schemaId ->
                I18n.UnknownSchemaId schemaId


viewCardIdLine : I18n.Language -> DataProxy a -> String -> Html msg
viewCardIdLine language data cardId =
    case Dict.get cardId data.cards of
        Just card ->
            viewCardLine language data card

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing card with ID: " ++ cardId) ]


viewCardLine : I18n.Language -> DataProxy a -> Card -> Html msg
viewCardLine language data card =
    text <| Strings.cardNameToString language data card


viewPropertyIdLine : I18n.Language -> Bool -> DataProxy a -> String -> Html msg
viewPropertyIdLine language independent data propertyId =
    case Dict.get propertyId data.properties of
        Just property ->
            viewPropertyLine language independent data property

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing property with ID: " ++ propertyId) ]


viewPropertyLine : I18n.Language -> Bool -> DataProxy a -> Property -> Html msg
viewPropertyLine language independent data property =
    -- The `independent` flag indicates whether to display the object of the property along with it key and value.
    let
        keyLabel =
            Dict.get property.keyId I18n.keyLabelById
                |> Maybe.map (I18n.translate language)
                |> Maybe.withDefault (Strings.idToString language data property.keyId)
    in
        div []
            [ if independent then
                div
                    (lineIdAttributes
                        language
                        Nothing
                        [ ( "ml-4", True ) ]
                        data
                        property.objectId
                    )
                    [ viewStatementIdLine
                        language
                        True
                        False
                        data
                        property.objectId
                    , viewStatementIdRatingPanel
                        language
                        Nothing
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
                        , ( if property.keyId == "con" then
                                "fa-minus"
                            else if property.keyId == "pro" then
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
            , div
                (lineIdAttributes
                    language
                    Nothing
                    [ ( "ml-4", True ) ]
                    data
                    property.valueId
                )
                [ viewStatementIdLine
                    language
                    True
                    False
                    data
                    property.valueId
                , viewStatementIdRatingPanel
                    language
                    Nothing
                    data
                    property.valueId
                ]
            ]


viewStatementIdLine : I18n.Language -> Bool -> Bool -> DataProxy a -> String -> Html msg
viewStatementIdLine language independent showDetails data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewCardLine language data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewPropertyLine language independent data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewValueTypeLine language showDetails data typedValue.value

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewValueIdLine : I18n.Language -> Bool -> DataProxy a -> String -> Html msg
viewValueIdLine language showDetails data valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueTypeLine language showDetails data typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueTypeLine : I18n.Language -> Bool -> DataProxy a -> ValueType -> Html msg
viewValueTypeLine language showDetails data valueType =
    if showDetails then
        div []
            [ i [] [ text (valueTypeToTypeLabel language valueType) ]
            , text (I18n.translate language I18n.Colon)
            , viewValueTypeLineContent language showDetails data valueType
            ]
    else
        viewValueTypeLineContent language showDetails data valueType


viewValueTypeLineContent : I18n.Language -> Bool -> DataProxy a -> ValueType -> Html msg
viewValueTypeLineContent language showDetails data valueType =
    case valueType of
        BooleanValue bool ->
            text (toString bool)

        EmailValue str ->
            aIfIsUrl [] str

        IdsArrayValue ids ->
            ul []
                (List.map
                    (\id ->
                        li
                            []
                            [ viewStatementIdLine language True showDetails data id ]
                    )
                    ids
                )

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

        -- LocalizedStringValue values ->
        --     if showDetails || Dict.size values > 1 then
        --         dl []
        --             (values
        --                 |> Dict.toList
        --                 |> List.concatMap
        --                     (\( languageCode, childValue ) ->
        --                         [ dt [] [ text languageCode ]
        --                         , dd [] [ aIfIsUrl [] childValue ]
        --                         ]
        --                     )
        --             )
        --     else
        --         div []
        --             (values
        --                 |> Dict.toList
        --                 |> List.map (\( languageCode, childValue ) -> aIfIsUrl [] childValue)
        --             )
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
