module Images exposing (..)

import Constants exposing (imagePathKeyIds)
import Dict exposing (Dict)
import I18n
import Strings
import Types exposing (..)
import Urls


idToImageUrl : I18n.Language -> DataProxy a -> String -> String
idToImageUrl language data id =
    case Dict.get id data.cards of
        Just card ->
            Strings.qualitiesToString imagePathKeyIds language data card
                |> Maybe.withDefault Urls.appLogoFullUrl

        Nothing ->
            case Dict.get id data.properties of
                Just property ->
                    Strings.qualitiesToString imagePathKeyIds language data property
                        |> Maybe.withDefault Urls.appLogoFullUrl

                Nothing ->
                    case Dict.get id data.values of
                        Just typedValue ->
                            Strings.qualitiesToString imagePathKeyIds
                                language
                                data
                                typedValue
                                |> Maybe.withDefault Urls.appLogoFullUrl

                        Nothing ->
                            Urls.appLogoFullUrl
