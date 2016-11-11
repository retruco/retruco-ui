module Person.Edit exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Types exposing (..)
import Views exposing (..)


type alias Model =
    PersonForm


type Msg
    = NameChanged String
    | TwitterNameChanged String


update : Msg -> String -> Model -> ( Model, Cmd Msg )
update msg fieldId model =
    case msg of
        NameChanged fieldValue ->
            ( { model | name = fieldValue }, Cmd.none )

        TwitterNameChanged fieldValue ->
            ( { model | twitterName = fieldValue }, Cmd.none )


view : String -> String -> Model -> FormErrors -> (Msg -> parentMsg) -> Html parentMsg
view fieldLabel fieldId model errors changed =
    let
        prefix =
            if String.isEmpty fieldId then
                fieldId
            else
                fieldId ++ "."
    in
        fieldset [ class "form-group" ]
            [ legend [] [ text fieldLabel ]
            , viewName "Name"
                (prefix ++ "name")
                model.name
                (Dict.get (prefix ++ "name") errors)
                (changed << NameChanged)
            , viewTwitterName "Twitter Name"
                (prefix ++ "twitter-name")
                model.twitterName
                (Dict.get (prefix ++ "twitterName") errors)
                (changed << TwitterNameChanged)
            ]
