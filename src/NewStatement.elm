module NewStatement exposing (init, Msg, Model, subscriptions, update, view)

import Authenticator.Model
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Person.Embed as PersonEmbed
import Requests exposing (..)
import String
import Task
import Types exposing (..)
import Views exposing (..)


-- MODEL


type alias Model = StatementForm


init : Model
init = initStatementForm


-- UPDATE


type Msg
    = CitedMsg PlainEmbed
    | Created DataIdBody
    | CreateError Http.Error
    | EventMsg EventEmbed
    | KindChanged String
    | LanguageCodeChanged String
    | NameChanged String
    | PersonMsg PersonEmbed.Msg
    | Submit
    | TwitterNameChanged String


update : Msg -> Maybe Authenticator.Model.Authentication -> Model -> ( Model, Cmd Msg, Maybe DataId )
update msg authenticationMaybe model =
    case msg of
        CitedMsg plainEmbed ->
            ({ model | cited = plainEmbed }, Cmd.none, Nothing)

        Created body ->
            (model, Cmd.none, Just body.data)

        CreateError err ->
            let
                _ = Debug.log "New Statement Create Error" err
            in
                (model, Cmd.none, Nothing)

        EventMsg eventEmbed ->
            ({ model | event = eventEmbed }, Cmd.none, Nothing)

        KindChanged kind ->
            ({ model | kind = kind }, Cmd.none, Nothing)

        LanguageCodeChanged languageCode ->
            ({ model | languageCode = languageCode }, Cmd.none, Nothing)

        NameChanged name ->
            ({ model | name = name }, Cmd.none, Nothing)

        PersonMsg childMsg ->
            let
                (childModel, childEffect) = PersonEmbed.update childMsg "person" model.person
            in
            ({ model | person = childModel }, Cmd.map PersonMsg childEffect, Nothing)

        Submit ->
            let
                errorsList = ( List.filterMap (
                    \(name, errorMaybe) ->
                        case errorMaybe of
                            Just error ->
                                Just (name, error)
                            Nothing ->
                                Nothing
                    )
                    [
                        ( "kind"
                        , if String.isEmpty model.kind
                            then Just "Missing type"
                            else Nothing
                        )
                    ,
                        ( "languageCpde"
                        , if  model.kind == "PlainStatement" && String.isEmpty model.languageCode
                            then Just "Missing language"
                            else Nothing
                        )
                    ,
                        ( "name"
                        , if List.member model.kind ["Event", "Person", "PlainStatement", "Tag"]
                            && String.isEmpty model.name
                            then Just "Missing name"
                            else Nothing
                        )
                    ] )

                cmd =
                    if List.isEmpty errorsList then
                        case authenticationMaybe of
                            Just authentication ->
                                Task.perform
                                    CreateError
                                    Created
                                    (newTaskCreateStatement
                                        authentication
                                        (convertStatementFormToCustom model))
                            Nothing ->
                                Cmd.none
                    else
                        Cmd.none
            in
                ({ model | errors = Dict.fromList errorsList }, cmd, Nothing)

        TwitterNameChanged twitterName ->
            ({ model | twitterName = twitterName }, Cmd.none, Nothing)


-- VIEW


view : Model -> Html Msg
view model =
    Html.form [ onSubmit Submit ]
        ([ viewKind model.kind (Dict.get "kind" model.errors) KindChanged ]
        ++
        (case model.kind of
            "Card" ->
                []

            "Citation" ->
                -- [ viewPlain model.cited (filterPrefix "cited." model.errors) CitedMsg
                -- , PersonEmbed.view "Person" "person" model.person model.errors PersonMsg
                -- , viewEvent model.event (filterPrefix "eventCited." model.errors) EventMsg
                -- ]
                [ PersonEmbed.view "Person" "person" model.person model.errors PersonMsg
                ]

            "Event" ->
                [ viewName "Name" "name" model.name (Dict.get "name" model.errors) NameChanged
                ]

            "Person" ->
                [ viewName "Name" "name" model.name (Dict.get "name" model.errors) NameChanged
                , viewTwitterName "Twitter Name" "twitter-name" model.twitterName (Dict.get "twitterName" model.errors)
                    TwitterNameChanged
                ]
            "PlainStatement" ->
                [ viewLanguageCode model.languageCode (Dict.get "languageCode" model.errors) LanguageCodeChanged
                , viewName "Name" "name" model.name (Dict.get "name" model.errors) NameChanged
                ]

            "Tag" ->
                [ viewName "Name" "name" model.name (Dict.get "name" model.errors) NameChanged
                ]

            _ ->
                []
        )
        ++
        [ button
            [ class "btn btn-primary", type' "submit" ]
            [ text "Create" ]
        ])


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PersonMsg (PersonEmbed.subscriptions model.person)
        ]
