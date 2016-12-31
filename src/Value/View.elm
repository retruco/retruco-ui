module Value.View exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Types exposing (..)
import Value.Types exposing (..)
import Views
import WebData


view : Model -> Html Msg
view model =
    let
        language =
            model.language
    in
        Views.viewWebData
            language
            (\loadingStatus ->
                case loadingStatus of
                    WebData.Loading _ ->
                        div [ class "text-center" ]
                            [ Views.viewLoading language ]

                    WebData.Loaded body ->
                        div []
                            [ viewLoaded language body.data ]
            )
            model.webData


viewLoaded : I18n.Language -> DataId -> Html Msg
viewLoaded language data =
    viewValueId language data data.id


viewValueId : I18n.Language -> DataId -> String -> Html Msg
viewValueId language data valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueType language data typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueType : I18n.Language -> DataId -> ValueType -> Html Msg
viewValueType language data valueType =
    let
        -- cardLink cardId =
        --     case Dict.get cardId cards of
        --         Nothing ->
        --             text ("Error: target card not found for ID: " ++ cardId)
        --         Just card ->
        --             let
        --                 linkText =
        --                     case I18n.getOneString language nameKeys card values of
        --                         Nothing ->
        --                             cardId
        --                         Just name ->
        --                             name
        --                 path =
        --                     Urls.pathForCard card
        --             in
        --                 aForPath navigate language path [] [ text linkText ]
        showLanguage =
            True
    in
        case valueType of
            BijectiveCardReferenceValue { targetId } ->
                -- cardLink targetId
                text targetId

            BooleanValue bool ->
                text (toString bool)

            CardIdArrayValue childValues ->
                ul [ class "list-unstyled" ]
                    (List.map
                        -- (\childValue -> li [] [ viewValueType language cards values showLanguage (CardIdValue childValue) ])
                        text
                        childValues
                    )

            CardIdValue cardId ->
                -- cardLink cardId
                text cardId

            LocalizedStringValue values ->
                let
                    viewString languageCode string =
                        if showLanguage || Dict.size values > 1 then
                            [ dt [] [ text languageCode ]
                            , dd [] [ aIfIsUrl [] string ]
                            ]
                        else
                            [ aIfIsUrl [] string ]
                in
                    dl []
                        (values
                            |> Dict.toList
                            |> List.concatMap (\( languageCode, childValue ) -> viewString languageCode childValue)
                        )

            NumberValue float ->
                text (toString float)

            StringValue str ->
                aIfIsUrl [] str

            ValueIdArrayValue childValues ->
                ul [ class "list-unstyled" ]
                    (List.map
                        (\childValue -> li [] [ viewValueType language data (ValueIdValue childValue) ])
                        childValues
                    )

            ValueIdValue valueId ->
                case Dict.get valueId data.values of
                    Nothing ->
                        text ("Error: referenced value not found for valueId: " ++ valueId)

                    Just subValue ->
                        viewValueType language data subValue.value

            WrongValue str schemaId ->
                div []
                    [ p [ style [ ( "color", "red" ) ] ] [ text "Wrong value!" ]
                    , pre [] [ text str ]
                    , p [] [ text ("schemaId: " ++ schemaId) ]
                    ]
