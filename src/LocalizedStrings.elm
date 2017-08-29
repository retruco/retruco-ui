module LocalizedStrings exposing (..)

import Constants exposing (nameKeyIds)
import Dict exposing (Dict)
import I18n
import Set exposing (Set)
import Types exposing (..)


cardNameToString : I18n.Language -> DataProxy a -> Card -> String
cardNameToString language data card =
    cardNameToStringForLanguages (getPreferredLanguages language) data card
        |> Maybe.withDefault (I18n.translate language <| I18n.UntitledCard card.id)


cardNameToStringForLanguages : List (Maybe I18n.Language) -> DataProxy a -> Card -> Maybe String
cardNameToStringForLanguages languages data card =
    statementPropertiesToStringForLanguages nameKeyIds languages data card


getPreferredLanguages : I18n.Language -> List (Maybe I18n.Language)
getPreferredLanguages language =
    if language == I18n.English then
        [ Just I18n.English, Nothing ]
    else
        [ Just language, Just I18n.English, Nothing ]


idsToStringForLanguage : Maybe I18n.Language -> DataProxy a -> Set String -> List String -> Maybe String
idsToStringForLanguage language data visitedIdsSet ids =
    case ids of
        id :: remainingIds ->
            case idToStringForLanguage language data visitedIdsSet id of
                Nothing ->
                    idsToStringForLanguage language data visitedIdsSet remainingIds

                justAString ->
                    justAString

        [] ->
            Nothing


idToString : I18n.Language -> DataProxy a -> String -> String
idToString language data id =
    case Dict.get id data.cards of
        Just card ->
            cardNameToString language data card

        Nothing ->
            case Dict.get id data.properties of
                Just property ->
                    propertyToString language data property

                Nothing ->
                    case Dict.get id data.values of
                        Just typedValue ->
                            typedValueToString language data typedValue

                        Nothing ->
                            I18n.translate language <| I18n.UnknownId id


idToStringForLanguage : Maybe I18n.Language -> DataProxy a -> Set String -> String -> Maybe String
idToStringForLanguage language data visitedIdsSet id =
    case Dict.get id data.cards of
        Just card ->
            cardNameToStringForLanguages [ language ] data card

        Nothing ->
            case Dict.get id data.properties of
                Just property ->
                    Just
                        ((idToStringForLanguage language data visitedIdsSet property.objectId
                            |> Maybe.withDefault
                                (I18n.translate
                                    (language |> Maybe.withDefault I18n.English)
                                 <|
                                    I18n.UnknownId property.objectId
                                )
                         )
                            ++ "\n"
                            ++ (idToStringForLanguage language data visitedIdsSet property.keyId
                                    |> Maybe.withDefault
                                        (I18n.translate
                                            (language |> Maybe.withDefault I18n.English)
                                         <|
                                            I18n.UnknownId property.keyId
                                        )
                               )
                            ++ "\n"
                            ++ (idToStringForLanguage language data visitedIdsSet property.valueId
                                    |> Maybe.withDefault
                                        (I18n.translate
                                            (language |> Maybe.withDefault I18n.English)
                                         <|
                                            I18n.UnknownId property.valueId
                                        )
                               )
                        )

                Nothing ->
                    case Dict.get id data.values of
                        Just typedValue ->
                            typedValueToStringForLanguage language data visitedIdsSet typedValue

                        Nothing ->
                            Nothing


idToStringForLanguages : List (Maybe I18n.Language) -> DataProxy a -> String -> Maybe String
idToStringForLanguages languages data id =
    case languages of
        language :: remainingLanguages ->
            case idToStringForLanguage language data Set.empty id of
                Nothing ->
                    idToStringForLanguages remainingLanguages data id

                justAString ->
                    justAString

        [] ->
            Nothing


propertyToString : I18n.Language -> DataProxy a -> Property -> String
propertyToString language data property =
    (idToString language data property.objectId)
        ++ "\n"
        ++ (idToString language data property.keyId)
        ++ " "
        ++ (idToString language data property.valueId)


statementPropertiesToString : List String -> I18n.Language -> DataProxy a -> Statement b -> Maybe String
statementPropertiesToString keyIds language data statement =
    statementPropertiesToStringForLanguages keyIds (getPreferredLanguages language) data statement


statementPropertiesToStringForLanguage :
    List String
    -> Maybe I18n.Language
    -> DataProxy a
    -> Statement b
    -> Maybe String
statementPropertiesToStringForLanguage keyIds language data statement =
    case keyIds of
        keyId :: remainingKeyIds ->
            case Dict.get keyId statement.properties of
                Just valueIds ->
                    case idsToStringForLanguage language data Set.empty valueIds of
                        Nothing ->
                            statementPropertiesToStringForLanguage remainingKeyIds language data statement

                        justAString ->
                            justAString

                Nothing ->
                    statementPropertiesToStringForLanguage remainingKeyIds language data statement

        [] ->
            Nothing


statementPropertiesToStringForLanguages :
    List String
    -> List (Maybe I18n.Language)
    -> DataProxy a
    -> Statement b
    -> Maybe String
statementPropertiesToStringForLanguages keyIds languages data statement =
    case languages of
        language :: remainingLanguages ->
            case statementPropertiesToStringForLanguage keyIds language data statement of
                Nothing ->
                    statementPropertiesToStringForLanguages keyIds remainingLanguages data statement

                justAString ->
                    justAString

        [] ->
            Nothing


typedStringValueToStringForLanguage :
    Maybe I18n.Language
    -> DataProxy a
    -> Set String
    -> TypedValue
    -> String
    -> Maybe String
typedStringValueToStringForLanguage language data visitedIdsSet typedValue string =
    case language of
        Just language ->
            case Dict.get (I18n.languageIdFromLanguage language) typedValue.properties of
                Just localizedValueIds ->
                    -- Found some strings localized for requested language.
                    idsToStringForLanguage Nothing data visitedIdsSet localizedValueIds

                Nothing ->
                    -- Look for requested localization from other localizations.
                    let
                        newVisitedIdsSet =
                            Set.insert typedValue.id visitedIdsSet

                        getStringFromOtherLanguages : List I18n.Language -> Maybe String
                        getStringFromOtherLanguages otherLanguages =
                            case otherLanguages of
                                otherLanguage :: remainingLanguages ->
                                    case Dict.get (I18n.languageIdFromLanguage otherLanguage) typedValue.properties of
                                        Just localizedValueIds ->
                                            case
                                                idsToStringForLanguage
                                                    (Just language)
                                                    data
                                                    newVisitedIdsSet
                                                    localizedValueIds
                                            of
                                                Nothing ->
                                                    getStringFromOtherLanguages remainingLanguages

                                                justAString ->
                                                    justAString

                                        Nothing ->
                                            getStringFromOtherLanguages remainingLanguages

                                [] ->
                                    Nothing
                    in
                        getStringFromOtherLanguages I18n.languages

        Nothing ->
            Just string


typedValueToString : I18n.Language -> DataProxy a -> TypedValue -> String
typedValueToString language data typedValue =
    typedValueToStringForLanguages (getPreferredLanguages language) data typedValue
        |> Maybe.withDefault (I18n.translate language <| I18n.UntitledTypedValue typedValue.id)


typedValueToStringForLanguage : Maybe I18n.Language -> DataProxy a -> Set String -> TypedValue -> Maybe String
typedValueToStringForLanguage language data visitedIdsSet typedValue =
    if Set.member typedValue.id visitedIdsSet then
        Nothing
    else
        case typedValue.value of
            EmailValue value ->
                typedStringValueToStringForLanguage language data visitedIdsSet typedValue value

            -- IdsArrayValue ids ->
            --     idsToStringForLanguage language data visitedIdsSet ids
            StringValue value ->
                typedStringValueToStringForLanguage language data visitedIdsSet typedValue value

            UrlValue value ->
                typedStringValueToStringForLanguage language data visitedIdsSet typedValue value

            value ->
                case language of
                    Just language ->
                        Nothing

                    Nothing ->
                        Just <| toString value


typedValueToStringForLanguages : List (Maybe I18n.Language) -> DataProxy a -> TypedValue -> Maybe String
typedValueToStringForLanguages languages data typedValue =
    case languages of
        language :: remainingLanguages ->
            case typedValueToStringForLanguage language data Set.empty typedValue of
                Nothing ->
                    typedValueToStringForLanguages remainingLanguages data typedValue

                justAString ->
                    justAString

        [] ->
            Nothing
