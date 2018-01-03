module Statements.Lines exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Statements.Ratings exposing (viewStatementIdRatingBadges)
import Strings
import Types exposing (..)
import Urls


valueWrapperToTypeLabel : I18n.Language -> ValueWrapper -> String
valueWrapperToTypeLabel language valueWrapper =
    I18n.translate language <|
        case valueWrapper of
            BooleanWrapper _ ->
                I18n.Boolean

            IdsArrayWrapper _ ->
                I18n.IdsArray

            EmailWrapper _ ->
                I18n.Email

            ImagePathWrapper _ ->
                I18n.Image

            NumberWrapper _ ->
                I18n.Number

            StringWrapper _ ->
                I18n.String

            UrlWrapper _ ->
                I18n.Url

            WrongWrapper _ schemaId ->
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


viewPropertyLine : Bool -> I18n.Language -> (String -> msg) -> Bool -> DataProxy a -> Property -> Html msg
viewPropertyLine embed language navigateMsg independent data property =
    -- The `independent` flag indicates whether to display the object of the property along with it key and value.
    let
        keyLabel =
            Dict.get property.keyId I18n.debateKeyLabelById
                |> Maybe.map (I18n.translate language)
                |> Maybe.withDefault (Strings.idToString language data property.keyId)
    in
        div []
            [ if independent then
                viewStatementIdRatedLine
                    div
                    embed
                    language
                    True
                    navigateMsg
                    [ ( "bg-white", True ), ( "ml-4", True ) ]
                    True
                    data
                    property.objectId
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
                , em [] [ text keyLabel ]
                ]
            , viewStatementIdRatedLine
                div
                embed
                language
                independent
                navigateMsg
                [ ( "bg-white", True ), ( "ml-4", True ) ]
                True
                data
                property.valueId
            ]


viewStatementIdLine : Bool -> I18n.Language -> (String -> msg) -> Bool -> Bool -> DataProxy a -> String -> Html msg
viewStatementIdLine embed language navigateMsg independent showDetails data statementId =
    case Dict.get statementId data.cards of
        Just card ->
            viewCardLine language data card

        Nothing ->
            case Dict.get statementId data.properties of
                Just property ->
                    viewPropertyLine embed language navigateMsg independent data property

                Nothing ->
                    case Dict.get statementId data.values of
                        Just typedValue ->
                            viewValueWrapperLine embed language navigateMsg showDetails data typedValue.value

                        Nothing ->
                            i [ class "text-warning" ] [ text (I18n.translate language <| I18n.UnknownId statementId) ]


viewStatementIdRatedLine :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Bool
    -> I18n.Language
    -> Bool
    -> (String -> msg)
    -> List ( String, Bool )
    -> Bool
    -> DataProxy a
    -> String
    -> Html msg
viewStatementIdRatedLine element embed language isLink navigateMsg classItems independant data id =
    let
        lineViewNodes =
            [ viewStatementIdLine
                embed
                language
                navigateMsg
                independant
                False
                data
                id
            , viewStatementIdRatingBadges language data id
            ]
    in
        if isLink then
            element
                [ classList
                    classItems
                ]
                [ aForPath
                    navigateMsg
                    embed
                    language
                    (Urls.idToPath data id)
                    [ classList
                        [ ( "align", True )
                        , ( "align-items-top", True )
                        , ( "d-flex", True )
                        , ( "flex-nowrap", True )
                        , ( "justify-content-between", True )
                        , ( "text-dark", True )
                        ]
                    ]
                    lineViewNodes
                ]
        else
            element
                [ classList
                    ([ ( "align", True )
                     , ( "align-items-top", True )
                     , ( "d-flex", True )
                     , ( "flex-nowrap", True )
                     , ( "justify-content-between", True )
                     ]
                        ++ classItems
                    )
                ]
                lineViewNodes


viewStatementIdRatedListGroupLine :
    Bool
    -> I18n.Language
    -> (String -> msg)
    -> String
    -> List ( String, Bool )
    -> Bool
    -> DataProxy a
    -> String
    -> Html msg
viewStatementIdRatedListGroupLine embed language navigateMsg path classItems independant data id =
    aForPath
        navigateMsg
        embed
        language
        (Urls.idToPath data id ++ path)
        [ classList
            ([ ( "align", True )
             , ( "align-items-top", True )
             , ( "d-flex", True )
             , ( "flex-nowrap", True )
             , ( "justify-content-between", True )
             , ( "lead", True )
             , ( "list-group-item", True )
             , ( "list-group-item-action", True )
             ]
                ++ classItems
            )
        ]
        [ viewStatementIdLine
            embed
            language
            navigateMsg
            independant
            False
            data
            id
        , viewStatementIdRatingBadges language data id
        ]


viewValueIdLine : Bool -> I18n.Language -> (String -> msg) -> Bool -> DataProxy a -> String -> Html msg
viewValueIdLine embed language navigateMsg showDetails data valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueWrapperLine embed language navigateMsg showDetails data typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueWrapperLine : Bool -> I18n.Language -> (String -> msg) -> Bool -> DataProxy a -> ValueWrapper -> Html msg
viewValueWrapperLine embed language navigateMsg showDetails data valueWrapper =
    if showDetails then
        div []
            [ i [] [ text (valueWrapperToTypeLabel language valueWrapper) ]
            , text (I18n.translate language I18n.Colon)
            , viewValueWrapperLineContent embed language navigateMsg showDetails data valueWrapper
            ]
    else
        viewValueWrapperLineContent embed language navigateMsg showDetails data valueWrapper


viewValueWrapperLineContent : Bool -> I18n.Language -> (String -> msg) -> Bool -> DataProxy a -> ValueWrapper -> Html msg
viewValueWrapperLineContent embed language navigateMsg showDetails data valueWrapper =
    case valueWrapper of
        BooleanWrapper bool ->
            text (toString bool)

        EmailWrapper str ->
            aIfIsUrl [] str

        IdsArrayWrapper ids ->
            ul []
                (List.map
                    (\id ->
                        li
                            []
                            [ viewStatementIdLine embed language navigateMsg True showDetails data id ]
                    )
                    ids
                )

        ImagePathWrapper path ->
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

        -- LocalizedStringWrapper values ->
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
        NumberWrapper float ->
            text (toString float)

        StringWrapper str ->
            aIfIsUrl [] str

        UrlWrapper str ->
            aIfIsUrl [] str

        WrongWrapper str schemaId ->
            div []
                [ p [ style [ ( "color", "red" ) ] ] [ text "Wrong value!" ]
                , pre [] [ text str ]
                , p [] [ text ("schemaId: " ++ schemaId) ]
                ]
