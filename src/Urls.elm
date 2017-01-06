module Urls exposing (..)

import Configuration
import Erl
import Http
import I18n
import List.Extra
import Navigation
import Regex


appLogoFullUrl : String
appLogoFullUrl =
    Configuration.appUrl ++ "img/ogptoolbox-logo.png"


idRegex : Regex.Regex
idRegex =
    Regex.regex "(^|/)(\\d+)(\\?|$)"


fullApiUrl : String -> String
fullApiUrl url =
    let
        parsedApiUrl =
            Erl.parse Configuration.apiUrl

        parsedUrl =
            Erl.parse <| String.trim url
    in
        if List.isEmpty <| List.filter (not << String.isEmpty) parsedUrl.host then
            Erl.toString <|
                { parsedUrl
                    | host = parsedApiUrl.host
                    , port_ = parsedApiUrl.port_
                    , protocol = parsedApiUrl.protocol
                }
        else
            url


fullUrl : String -> String
fullUrl url =
    let
        parsedAppUrl =
            Erl.parse Configuration.appUrl

        parsedUrl =
            Erl.parse <| String.trim url
    in
        if List.isEmpty <| List.filter (not << String.isEmpty) parsedUrl.host then
            Erl.toString <|
                { parsedUrl
                    | host = parsedAppUrl.host
                    , port_ = parsedAppUrl.port_
                    , protocol = parsedAppUrl.protocol
                }
        else
            url


languagePath : I18n.Language -> String -> String
languagePath language path =
    "/" ++ (I18n.iso639_1FromLanguage language) ++ path


paramsToQuery : List ( String, Maybe String ) -> String
paramsToQuery params =
    let
        query =
            params
                |> List.filterMap
                    (\( key, value ) ->
                        case value of
                            Just value ->
                                Just <| Http.encodeUri key ++ "=" ++ Http.encodeUri value

                            Nothing ->
                                Nothing
                    )
                |> String.join "&"
    in
        if String.isEmpty query then
            ""
        else
            "?" ++ query


parentUrl : String -> String
parentUrl url =
    let
        parsedUrl =
            Erl.parse url
    in
        if List.isEmpty <| List.filter (not << String.isEmpty) parsedUrl.path then
            url
        else
            Erl.toString <| { parsedUrl | path = List.Extra.init parsedUrl.path |> Maybe.withDefault [] }


querySearchTerm : Navigation.Location -> String
querySearchTerm location =
    querySingleParameter "q" location
        |> Maybe.withDefault ""


querySingleParameter : String -> Navigation.Location -> Maybe String
querySingleParameter key location =
    (Erl.parse location.href).query
        |> List.filter (\( k, v ) -> k == key)
        |> List.map (\( k, v ) -> v)
        |> List.head


queryStringForParams : List String -> Navigation.Location -> String
queryStringForParams params location =
    let
        keptQuery =
            (Erl.parse location.href).query
                |> List.filter (\( k, v ) -> List.member k params)
    in
        if List.isEmpty keptQuery then
            ""
        else
            let
                encodedTuples =
                    List.map (\( x, y ) -> ( Http.encodeUri x, Http.encodeUri y )) keptQuery

                parts =
                    List.map (\( a, b ) -> a ++ "=" ++ b) encodedTuples
            in
                "?" ++ (String.join "&" parts)


replaceLanguageInLocation : I18n.Language -> Navigation.Location -> String
replaceLanguageInLocation language location =
    let
        url =
            Erl.parse location.href

        path =
            List.tail url.path
                |> Maybe.withDefault []
                |> (::) (I18n.iso639_1FromLanguage language)

        newUrl =
            { url | path = path }
    in
        Erl.toString newUrl


urlToId : String -> Maybe String
urlToId url =
    (Regex.find Regex.All idRegex url
        |> List.head
    )
        |> Maybe.andThen
            (\match ->
                case match.submatches |> List.drop 1 |> List.head of
                    Nothing ->
                        Nothing

                    Just maybe ->
                        maybe
            )
