module About.State exposing (..)

import About.Types exposing (..)
import Authenticator.Types exposing (Authentication)
import I18n
import Navigation
import Ports
import Urls


init : Maybe Authentication -> I18n.Language -> Model
init authentication language =
    { authentication = authentication
    , language = language
    }


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate _ model =
    let
        language =
            model.language
    in
        ( model
        , Ports.setDocumentMetadata
            { description = I18n.translate language I18n.AboutDescription
            , imageUrl = Urls.appLogoFullUrl
            , title = I18n.translate language I18n.About
            }
        )
