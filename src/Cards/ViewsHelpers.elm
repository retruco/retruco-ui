module Cards.ViewsHelpers exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import I18n
import Types exposing (..)
import Values.ViewsHelpers exposing (..)


viewCard : I18n.Language -> (String -> msg) -> DataProxy a -> Card -> Html msg
viewCard language navigate data card =
    let
        cardName =
            I18n.getOneString language nameKeys card data.values
                |> Maybe.withDefault card.id

        values =
            data.values

        viewCardArgumentsItem argument =
            li [ class "list-group-item" ]
                [ span
                    [ ariaHidden True
                    , class
                        ("fa fa-fw "
                            ++ if argument.keyId == "cons" then
                                "fa-minus"
                               else if argument.keyId == "pros" then
                                "fa-plus"
                               else
                                "fa-circle"
                        )
                    ]
                    []
                , text " "
                , div [ style [ ( "display", "inline-block" ) ] ]
                    [ viewValueIdLine
                        language
                        (Just navigate)
                        data
                        False
                        argument.valueId
                    ]

                -- , text " "
                -- , if Configuration.deepDebate then
                --     button
                --         [ attribute "data-target" "#debate-content"
                --         , attribute "data-toggle" "modal"
                --         , class "btn btn-default btn-xs btn-action"
                --         , onClick
                --             (ForSelf
                --                 (LoadDebateProperties
                --                     [ argument.valueId ]
                --                 )
                --             )
                --         , type_ "button"
                --         ]
                --         [ text (I18n.translate language (I18n.Debate)) ]
                --   else
                --     text ""
                ]

        viewCardPropertiesItem keyId valueId =
            li [ class "list-group-item" ]
                [ case Dict.get keyId values of
                    Nothing ->
                        text ("Error: value not found for key: " ++ keyId)

                    Just keyValue ->
                        viewValueTypeLine language (Just navigate) data False keyValue.value
                , text <| I18n.translate language I18n.Colon
                , case Dict.get valueId values of
                    Nothing ->
                        text ("Error: value not found for value: " ++ valueId)

                    Just valueValue ->
                        viewValueTypeLine language (Just navigate) data False valueValue.value
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
            , hr [] []
            , h2 [] [ text <| I18n.translate language I18n.Arguments ]
            , ul [ class "list-group" ]
                (card.arguments
                    |> List.map viewCardArgumentsItem
                )
            ]


viewCardId : I18n.Language -> (String -> msg) -> DataProxy a -> String -> Html msg
viewCardId language navigate data cardId =
    case Dict.get cardId data.cards of
        Just card ->
            viewCard language navigate data card

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing card with ID: " ++ cardId) ]
