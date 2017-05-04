module Statements.ViewsHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Helpers exposing (aForPath)
import I18n
import Types exposing (Argument, DataProxy)
import Values.ViewsHelpers


viewArgumentsBlock : I18n.Language -> (String -> msg) -> DataProxy a -> String -> String -> List Argument -> Html msg
viewArgumentsBlock language navigate data objectsUrlName objectId arguments =
    let
        viewArgument argument =
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
                    , Values.ViewsHelpers.viewValueIdLine
                        language
                        (Just navigate)
                        data
                        False
                        argument.valueId
                    ]
                , -- TODO
                  aForPath
                    navigate
                    language
                    ("/" ++ "TODO-type" ++ "/" ++ "TODO-id" ++ "/arguments")
                    [ class "btn btn-secondary" ]
                    [ text (I18n.translate language (I18n.Debate)) ]
                ]
    in
        div []
            [ h2 [ class "d-flex justify-content-between" ]
                [ span [] [ text <| I18n.translate language I18n.Arguments ]
                , aForPath
                    navigate
                    language
                    ("/" ++ objectsUrlName ++ "/" ++ objectId ++ "/arguments")
                    [ class "btn btn-secondary" ]
                    [ text (I18n.translate language (I18n.Debate)) ]
                ]
            , ul [ class "list-group" ]
                (arguments
                    |> List.map viewArgument
                )
            ]
