module Values.Autocomplete.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import I18n
import Values.Autocomplete.Types exposing (..)
import Views exposing (errorInfos)


viewAutocomplete :
    I18n.Language
    -> String
    -> I18n.TranslationId
    -> I18n.TranslationId
    -> Maybe I18n.TranslationId
    -> Model
    -> Html Msg
viewAutocomplete language parentId controlLabelI18n controlPlaceholderI18n error model =
    let
        controlId =
            if String.isEmpty parentId then
                "autocomplete"
            else
                parentId ++ ".autocomplete"

        controlFindAnother =
            I18n.translate language I18n.FindAnotherValue

        controlFindOne =
            I18n.translate language I18n.FindValue

        controlLabel =
            I18n.translate language controlLabelI18n

        controlPlaceholder =
            I18n.translate language controlPlaceholderI18n

        controlSelectOneOrTypeMoreCharacters =
            I18n.translate language I18n.SelectValueOrTypeMoreCharacters

        controlTitle =
            I18n.translate language I18n.EnterValue

        ( errorClass, errorAttributes, errorBlock ) =
            errorInfos language controlId error

        menuId =
            controlId ++ "-menu"

        showAutocompleter =
            case model.autocompleterState of
                AutocompleterVisible ->
                    True

                _ ->
                    False
    in
        div [ class ("form-group" ++ errorClass) ]
            (List.concat
                [ [ label [ class "control-label", for controlId ] [ text controlLabel ]
                  , div [ class "input-group" ]
                        [ span [ class "input-group-addon" ]
                            [ input
                                [ ariaLabel <| I18n.translate language I18n.RadioButtonForFollowingAutocompleter
                                , checked <| model.selected == Nothing
                                , disabled (not showAutocompleter || (List.isEmpty model.autocompletions))
                                , onClick (ForSelf <| Select Nothing)
                                , type_ "radio"
                                ]
                                []
                            ]

                        -- , input
                        --     (List.concat
                        --         [ [ attribute "aria-autocomplete" "list"
                        --           , ariaExpanded <| String.toLower <| toString showAutocompleter
                        --           , attribute "aria-haspopup" <| String.toLower <| toString showAutocompleter
                        --           , attribute "aria-owns" menuId
                        --           , autocomplete False
                        --           , class "form-control"
                        --           , id controlId
                        --           , onInput (ForSelf << InputChanged)
                        --           , placeholder controlPlaceholder
                        --           , title controlTitle
                        --           , role "combobox"
                        --           , type_ "text"
                        --           , value query
                        --           ]
                        --         , (case model.selected of
                        --             Just selected ->
                        --                 [ ariaActiveDescendant selected.autocomplete ]
                        --             Nothing ->
                        --                 []
                        --           )
                        --         , errorAttributes
                        --         ]
                        --     )
                        --     []
                        , textarea
                            (List.concat
                                [ [ attribute "aria-autocomplete" "list"
                                  , ariaExpanded <| String.toLower <| toString showAutocompleter
                                  , attribute "aria-haspopup" <| String.toLower <| toString showAutocompleter
                                  , attribute "aria-owns" menuId
                                  , class "form-control"
                                  , id controlId
                                  , onInput (ForSelf << InputChanged)
                                  , placeholder controlPlaceholder
                                  , role "combobox"
                                  , title controlTitle
                                  , value model.autocomplete
                                  ]
                                , (case model.selected of
                                    Just selected ->
                                        [ ariaActiveDescendant selected.autocomplete ]

                                    Nothing ->
                                        []
                                  )
                                , errorAttributes
                                ]
                            )
                            []
                        ]
                  , span [ class "sr-only" ] [ text <| I18n.translate language I18n.LoadingMenu ]
                  ]
                , errorBlock
                , if showAutocompleter then
                    [ fieldset [ class "form-group" ]
                        ([ legend []
                            [ text <| I18n.translate language I18n.Suggestions ]
                         ]
                            ++ List.map
                                (\autocompletion ->
                                    div [ class "form-check" ]
                                        [ label [ class "form-check-label" ]
                                            [ input
                                                [ class "form-check-input"
                                                , checked <|
                                                    (case model.selected of
                                                        Just selected ->
                                                            selected.value.id == autocompletion.value.id

                                                        Nothing ->
                                                            False
                                                    )
                                                , onClick (ForSelf <| Select <| Just autocompletion.value.id)
                                                , type_ "radio"
                                                ]
                                                []
                                            , text " "
                                            , text autocompletion.autocomplete
                                            ]
                                        ]
                                )
                                model.autocompletions
                        )
                    ]
                  else
                    []
                ]
            )
