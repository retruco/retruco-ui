module Cards.Item.View exposing (..)

import Arguments.Index.View
import Cards.Item.Types exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import Http.Error
import I18n
import Properties.KeysAutocomplete.View
import SameKeyProperties.View
import Types exposing (..)
import Values.ViewsHelpers exposing (..)
import Views


view : Model -> Html Msg
view model =
    case ( model.argumentsModel, model.sameKeyPropertiesModel ) of
        ( Just argumentsModel, _ ) ->
            Arguments.Index.View.view argumentsModel
                |> Html.map translateArgumentsMsg

        ( _, Just sameKeyPropertiesModel ) ->
            SameKeyProperties.View.view sameKeyPropertiesModel
                |> Html.map translateSameKeyPropertiesMsg

        ( Nothing, Nothing ) ->
            let
                data =
                    model.data

                language =
                    model.language
            in
                case Dict.get model.id data.cards of
                    Just card ->
                        let
                            cardName =
                                I18n.getOneString language nameKeys card data.values
                                    |> Maybe.withDefault card.id

                            values =
                                data.values

                            viewCardArgumentsItem argument =
                                li [ class "list-group-item justify-content-between" ]
                                    [ div [ class "d-inline-flex" ]
                                        [ span
                                            [ ariaHidden True
                                            , class
                                                ("fa "
                                                    ++ (if argument.keyId == "cons" then
                                                            "fa-minus"
                                                        else if argument.keyId == "pros" then
                                                            "fa-plus"
                                                        else
                                                            "fa-circle"
                                                       )
                                                    ++ " fa-fw mr-2"
                                                )
                                            ]
                                            []
                                        , viewValueIdLine
                                            language
                                            (Just (ForParent << Navigate))
                                            data
                                            False
                                            argument.valueId
                                        ]
                                    , -- TODO
                                      aForPath
                                        (ForParent << Navigate)
                                        language
                                        ("/cards/" ++ model.id ++ "/arguments")
                                        [ class "btn btn-secondary" ]
                                        [ text (I18n.translate language (I18n.Debate)) ]
                                    ]

                            viewCardPropertiesItem keyId valueId =
                                li [ class "list-group-item justify-content-between" ]
                                    [ div [ class "d-inline-flex" ]
                                        [ case Dict.get keyId values of
                                            Nothing ->
                                                text ("Error: value not found for key: " ++ keyId)

                                            Just keyValue ->
                                                viewValueTypeLine
                                                    language
                                                    (Just (ForParent << Navigate))
                                                    data
                                                    False
                                                    keyValue.value
                                        , span [ class "mr-1" ] [ text <| I18n.translate language I18n.Colon ]
                                        , case Dict.get valueId values of
                                            Nothing ->
                                                text ("Error: value not found for value: " ++ valueId)

                                            Just valueValue ->
                                                viewValueTypeLine
                                                    language
                                                    (Just (ForParent << Navigate))
                                                    data
                                                    False
                                                    valueValue.value
                                        ]
                                    , aForPath
                                        (ForParent << Navigate)
                                        language
                                        ("/cards/" ++ model.id ++ "/properties/" ++ keyId)
                                        [ class "btn btn-secondary" ]
                                        [ text (I18n.translate language (I18n.Edit)) ]
                                    ]
                        in
                            div []
                                [ h1 [] [ text cardName ]
                                , h2 [] [ text <| I18n.translate language I18n.Properties ]
                                , ul [ class "list-group" ]
                                    (card.properties
                                        |> Dict.map viewCardPropertiesItem
                                        |> Dict.values
                                    )
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
                                , hr [] []
                                , h2 [ class "d-flex justify-content-between" ]
                                    [ span [] [ text <| I18n.translate language I18n.Arguments ]
                                    , aForPath
                                        (ForParent << Navigate)
                                        language
                                        ("/cards/" ++ model.id ++ "/arguments")
                                        [ class "btn btn-secondary" ]
                                        [ text (I18n.translate language (I18n.Debate)) ]
                                    ]
                                , ul [ class "list-group" ]
                                    (card.arguments
                                        |> List.map viewCardArgumentsItem
                                    )
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
                                            I18n.translate language I18n.CardRetrievalFailed
                                                ++ I18n.translate language I18n.Colon
                                        ]
                                    , text <| Http.Error.toString language httpError
                                    ]

                            Nothing ->
                                div [ class "text-center" ]
                                    [ Views.viewLoading language ]
