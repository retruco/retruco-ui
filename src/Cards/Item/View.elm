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
import LineViews exposing (viewStatementIdLine)
import Strings
import Properties.KeysAutocomplete.View
import SameKeyProperties.View
import Statements.ViewsHelpers exposing (viewDebatePropertiesBlock)
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
                case ( Dict.get model.id data.cards, model.debatePropertyIds ) of
                    ( Just card, Just debatePropertyIds ) ->
                        let
                            cardName =
                                Strings.cardNameToString language data card

                            values =
                                data.values

                            viewCardPropertiesItem keyId valueIds =
                                li [ class "list-group-item justify-content-between" ]
                                    [ div [ class "d-inline-flex" ]
                                        [ viewStatementIdLine
                                            language
                                            (Just (ForParent << Navigate))
                                            True
                                            False
                                            data
                                            keyId
                                        , span [ class "mr-1" ] [ text <| I18n.translate language I18n.Colon ]
                                        , case valueIds of
                                            [ valueId ] ->
                                                viewStatementIdLine
                                                    language
                                                    (Just (ForParent << Navigate))
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
                                                                    language
                                                                    (Just (ForParent << Navigate))
                                                                    True
                                                                    False
                                                                    data
                                                                    valueId
                                                                ]
                                                        )
                                                        valueIds
                                                    )
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
                                , viewDebatePropertiesBlock language (ForParent << Navigate) data debatePropertyIds
                                ]

                    ( _, _ ) ->
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
