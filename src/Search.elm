module Search exposing (init, InternalMsg, Model, MsgTranslation, MsgTranslator, translateMsg, update, view)

import Authenticator.Model
import Html exposing (a, br, button, div, footer, form, h1, h2, h3, h4, Html, img, input, label, li, p, span, text, u, ul)
import Html.Attributes exposing (action, alt, attribute, class, for, href, id, method, name, placeholder, src, title, type', value)


-- MODEL


type alias Model =
    {}


init : Model
init =
    {}


-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = Todo


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg = Msg -> parentMsg


navigate : String -> Msg
navigate path =
    ForParent (Navigate path)


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg {onInternalMsg, onNavigate} msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg


update : InternalMsg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg )
update msg authenticationMaybe model =
    case msg of
        Todo ->
            ( model, Cmd.none )


-- VIEW


view : Maybe Authenticator.Model.Authentication -> Model -> Html Msg
view authenticationMaybe model =
    div []
        [ text "Search TODO" ]
