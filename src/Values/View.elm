module Values.View exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Html.Helpers exposing (aForPath, aIfIsUrl)
import I18n
import Json.Decode
import Types exposing (..)
import Values.Types exposing (..)
import Views
import WebData


searchSortLabelCouples : List ( String, String )
searchSortLabelCouples =
    [ ( "first", "First" )
    , ( "latest", "Latest" )
    ]


searchSorts : List String
searchSorts =
    List.map (\( item, label ) -> item) searchSortLabelCouples


decodeSearchSort : String -> Json.Decode.Decoder String
decodeSearchSort value =
    if List.member value searchSorts then
        Json.Decode.succeed value
    else
        Json.Decode.fail ("Unknown search sort: " ++ value)


view : Model -> Html Msg
view model =
    let
        language =
            model.language
    in
        div []
            [ nav
                [ class "navbar navbar-light bg-faded" ]
                [ ul [ class "nav navbar-nav float-lg-right" ]
                    [ li [ class "nav-item" ]
                        [ aForPath
                            (ForParent << Navigate)
                            language
                            "/values/new"
                            [ class "btn btn-secondary", role "button" ]
                            [ text <| I18n.translate language I18n.NewValue ]
                        ]
                    ]
                , Html.form [ class "form-inline", onSubmit (ForSelf Submit) ]
                    [ viewInlineSearchSort
                        language
                        model.searchSort
                        (Dict.get "searchSort" model.errors)
                        (ForSelf << SearchSortChanged)
                    , Views.viewInlineSearchTerm
                        language
                        model.searchTerm
                        (Dict.get "searchTerm" model.errors)
                        (ForSelf << SearchTermChanged)
                    , button [ class "btn btn-primary", type_ "button" ]
                        [ span [ class "fa fa-search" ] []
                        , text " "
                        , text <| I18n.translate language I18n.Search
                        ]
                    ]
                ]
            , Views.viewWebData
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
            ]


viewInlineSearchSort : I18n.Language -> String -> Maybe String -> (String -> msg) -> Html msg
viewInlineSearchSort language searchSort errorMaybe searchSortChanged =
    let
        ( errorClass, errorAttributes, errorBlock ) =
            case errorMaybe of
                Just error ->
                    ( " has-danger"
                    , [ ariaDescribedby "search-sort-error" ]
                    , [ div
                            [ class "form-control-feedback"
                            , id "search-sort-error"
                            ]
                            [ text error ]
                      ]
                    )

                Nothing ->
                    ( "", [], [] )
    in
        div [ class ("form-group" ++ errorClass) ]
            ([ label [ class "sr-only", for "search-sort" ] [ text "Type" ]
             , select
                ([ class "form-control"
                 , id "search-sort"
                 , on "change" (Json.Decode.map searchSortChanged (targetValue |> Json.Decode.andThen decodeSearchSort))
                 ]
                    ++ errorAttributes
                )
                (List.map
                    (Views.viewOption searchSort)
                    searchSortLabelCouples
                )
             ]
                ++ errorBlock
            )


viewLoaded : I18n.Language -> DataIds -> Html Msg
viewLoaded language data =
    -- ul [ class "list-group" ]
    --     (List.map (\valueId -> li [ class "list-group-item" ] [ viewValueId language data valueId ]) data.ids)
    div [ class "list-group" ]
        (List.map
            (\valueId ->
                aForPath
                    (ForParent << Navigate)
                    language
                    ("/values/" ++ valueId)
                    [ class "list-group-item list-group-item-action" ]
                    [ viewValueId language data valueId ]
            )
            data.ids
        )


viewValueId : I18n.Language -> DataIds -> String -> Html Msg
viewValueId language data valueId =
    case Dict.get valueId data.values of
        Just typedValue ->
            viewValueType language data typedValue.value

        Nothing ->
            i [ class "text-warning" ] [ text ("Missing value with ID: " ++ valueId) ]


viewValueType : I18n.Language -> DataIds -> ValueType -> Html Msg
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
