module Image.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http.Error
import Image.Types exposing (..)
import I18n
import Urls


viewImageUploadStatus : I18n.Language -> ImageUploadStatus -> Html msg
viewImageUploadStatus language imageUploadStatus =
    let
        missingImage messageI18n =
            div [ class "text-center" ]
                [ span
                    [ attribute "aria-hidden" "true"
                    , class "fa fa-camera fa-4"
                    ]
                    []
                , p [] [ text <| I18n.translate language messageI18n ]
                ]
    in
        case imageUploadStatus of
            ImageNotUploadedStatus ->
                missingImage I18n.UploadImage

            ImageSelectedStatus ->
                missingImage I18n.ReadingSelectedImage

            ImageReadStatus { contents, filename } ->
                missingImage (I18n.UploadingImage filename)

            ImageUploadedStatus path ->
                figure
                    [ class "figure text-center"
                    , style [ ( "width", "100%" ) ]
                    ]
                    [ img
                        [ alt <| I18n.translate language I18n.ImageAlt
                        , class "figure-img img-fluid rounded"
                        , src (Urls.fullApiUrl path)
                        ]
                        []
                    , figcaption [ class "figure-caption" ] [ text path ]
                    ]

            ImageUploadErrorStatus httpError ->
                missingImage <|
                    I18n.ImageUploadError <|
                        Http.Error.toString language httpError
