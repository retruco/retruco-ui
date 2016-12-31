module Value.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Objects.ViewsParts exposing (..)
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
                            [ viewValueIdLine language (Just (ForParent << Navigate)) body.data body.data.id ]
            )
            model.webData
