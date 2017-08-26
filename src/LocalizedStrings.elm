module LocalizedStrings exposing (..)

import Constants exposing (nameKeyIds)
import Dict exposing (Dict)
import I18n
import Set exposing (Set)
import Types exposing (..)


getLocalizedCardName : List (Maybe I18n.Language) -> DataProxy a -> Card -> Maybe String
getLocalizedCardName languages data card =
    getLocalizedStringFromStatementProperties nameKeyIds languages data card


getLocalizedStringFromId : List (Maybe I18n.Language) -> DataProxy a -> String -> Maybe String
getLocalizedStringFromId languages data id =
    case languages of
        language :: remainingLanguages ->
            case getStringFromId language data Set.empty id of
                Nothing ->
                    getLocalizedStringFromId remainingLanguages data id

                justAString ->
                    justAString

        [] ->
            Nothing


getLocalizedStringFromStatementProperties :
    List String
    -> List (Maybe I18n.Language)
    -> DataProxy a
    -> Statement b
    -> Maybe String
getLocalizedStringFromStatementProperties keyIds languages data statement =
    case languages of
        language :: remainingLanguages ->
            case getStringFromStatementProperties keyIds language data statement of
                Nothing ->
                    getLocalizedStringFromStatementProperties keyIds remainingLanguages data statement

                justAString ->
                    justAString

        [] ->
            Nothing


getPreferredLanguages : I18n.Language -> List (Maybe I18n.Language)
getPreferredLanguages language =
    if language == I18n.English then
        [ Just I18n.English, Nothing ]
    else
        [ Just language, Just I18n.English, Nothing ]


getStringFromId : Maybe I18n.Language -> DataProxy a -> Set String -> String -> Maybe String
getStringFromId language data visitedIdsSet id =
    case Dict.get id data.cards of
        Just card ->
            getLocalizedCardName [ language ] data card

        Nothing ->
            case Dict.get id data.properties of
                Just property ->
                    Just
                        ((getStringFromId language data visitedIdsSet property.objectId
                            |> Maybe.withDefault
                                (I18n.translate
                                    (language |> Maybe.withDefault I18n.English)
                                 <|
                                    I18n.UnknownId property.objectId
                                )
                         )
                            ++ "\n"
                            ++ (getStringFromId language data visitedIdsSet property.keyId
                                    |> Maybe.withDefault
                                        (I18n.translate
                                            (language |> Maybe.withDefault I18n.English)
                                         <|
                                            I18n.UnknownId property.keyId
                                        )
                               )
                            ++ "\n"
                            ++ (getStringFromId language data visitedIdsSet property.valueId
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
                            getStringFromTypedValue language data visitedIdsSet typedValue

                        Nothing ->
                            Nothing


getStringFromIds : Maybe I18n.Language -> DataProxy a -> Set String -> List String -> Maybe String
getStringFromIds language data visitedIdsSet ids =
    case ids of
        id :: remainingIds ->
            case getStringFromId language data visitedIdsSet id of
                Nothing ->
                    getStringFromIds language data visitedIdsSet remainingIds

                justAString ->
                    justAString

        [] ->
            Nothing


getStringFromStatementProperties : List String -> Maybe I18n.Language -> DataProxy a -> Statement b -> Maybe String
getStringFromStatementProperties keyIds language data statement =
    case keyIds of
        keyId :: remainingKeyIds ->
            case Dict.get keyId statement.properties of
                Just valueIds ->
                    case getStringFromIds language data Set.empty valueIds of
                        Nothing ->
                            getStringFromStatementProperties remainingKeyIds language data statement

                        justAString ->
                            justAString

                Nothing ->
                    getStringFromStatementProperties remainingKeyIds language data statement

        [] ->
            Nothing


getStringFromTypedStringValue : Maybe I18n.Language -> DataProxy a -> Set String -> TypedValue -> String -> Maybe String
getStringFromTypedStringValue language data visitedIdsSet typedValue string =
    case language of
        Just language ->
            case Dict.get (I18n.languageIdFromLanguage language) typedValue.properties of
                Just localizedValueIds ->
                    -- Found some strings localized for requested language.
                    getStringFromIds Nothing data visitedIdsSet localizedValueIds

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
                                                getStringFromIds
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


getStringFromTypedValue : Maybe I18n.Language -> DataProxy a -> Set String -> TypedValue -> Maybe String
getStringFromTypedValue language data visitedIdsSet typedValue =
    if Set.member typedValue.id visitedIdsSet then
        Nothing
    else
        case typedValue.value of
            EmailValue value ->
                getStringFromTypedStringValue language data visitedIdsSet typedValue value

            -- IdsArrayValue ids ->
            --     getStringFromIds language data visitedIdsSet ids
            StringValue value ->
                getStringFromTypedStringValue language data visitedIdsSet typedValue value

            UrlValue value ->
                getStringFromTypedStringValue language data visitedIdsSet typedValue value

            value ->
                case language of
                    Just language ->
                        Nothing

                    Nothing ->
                        Just <| toString value
