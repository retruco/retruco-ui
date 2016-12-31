module Search exposing (init, InternalMsg, Model, MsgTranslation, MsgTranslator, translateMsg, update, view)

import Authenticator.Types exposing (Authentication)
import Dict exposing (Dict)
import Html exposing (a, br, button, div, footer, form, h1, h2, h3, h4, Html, img, input, label, li, nav, p, select, option, span, text, u, ul)
import Html.Attributes exposing (action, alt, attribute, class, for, href, id, method, name, placeholder, src, title, type_, value)
import Html.Events exposing (onSubmit)
import I18n
import String
import Types exposing (FormErrors, SearchCriteria)
import Views exposing (viewInlineSearchLanguageCode, viewInlineSearchSort, viewInlineSearchTerm, viewInlineSearchType)


-- MODEL


type alias Model =
    { errors : FormErrors
    , searchCriteria : SearchCriteria
    , searchLanguageCode : String
    , searchSort : String
    , searchTerm : String
    , searchType : String
    }


init : Model
init =
    { errors = Dict.empty
    , searchCriteria =
        { kinds = [ "Citation", "Event", "Person", "PlainStatement" ]
        , languageCodeMaybe = Nothing
        , sort = "Popular"
        , termMaybe = Nothing
        }
    , searchLanguageCode = ""
    , searchSort = "Popular"
    , searchTerm = ""
    , searchType = ""
    }



-- UPDATE


type ExternalMsg
    = Navigate String


type InternalMsg
    = SearchLanguageCodeChanged String
    | SearchSortChanged String
    | SearchTermChanged String
    | SearchTypeChanged String
    | Submit


type Msg
    = ForParent ExternalMsg
    | ForSelf InternalMsg


type alias MsgTranslation parentMsg =
    { onInternalMsg : InternalMsg -> parentMsg
    , onNavigate : String -> parentMsg
    }


type alias MsgTranslator parentMsg =
    Msg -> parentMsg


convertControlsToSearchCriteria : Model -> Result FormErrors SearchCriteria
convertControlsToSearchCriteria model =
    let
        errorsList =
            (List.filterMap
                (\( name, errorMaybe ) ->
                    case errorMaybe of
                        Just error ->
                            Just ( name, error )

                        Nothing ->
                            Nothing
                )
                [ ( "searchLanguageCode"
                  , Nothing
                  )
                , ( "searchSort"
                  , if String.isEmpty model.searchSort then
                        Just "Missing sort criteria"
                    else
                        Nothing
                  )
                , ( "searchTerm"
                  , Nothing
                  )
                , ( "searchType"
                  , Nothing
                  )
                ]
            )
    in
        if List.isEmpty errorsList then
            Ok
                { kinds =
                    if String.isEmpty model.searchType then
                        [ "Citation", "Event", "Person", "PlainStatement" ]
                    else
                        [ model.searchType ]
                , languageCodeMaybe =
                    if String.isEmpty model.searchLanguageCode then
                        Nothing
                    else
                        Just model.searchLanguageCode
                , sort = model.searchSort
                , termMaybe =
                    if String.isEmpty model.searchTerm then
                        Nothing
                    else
                        Just model.searchTerm
                }
        else
            Err (Dict.fromList errorsList)


translateMsg : MsgTranslation parentMsg -> MsgTranslator parentMsg
translateMsg { onInternalMsg, onNavigate } msg =
    case msg of
        ForParent (Navigate path) ->
            onNavigate path

        ForSelf internalMsg ->
            onInternalMsg internalMsg


update : InternalMsg -> Maybe Authentication -> Model -> ( Model, Cmd Msg )
update msg authentication model =
    case msg of
        SearchLanguageCodeChanged searchLanguageCode ->
            ( { model | searchLanguageCode = searchLanguageCode }, Cmd.none )

        SearchSortChanged searchSort ->
            ( { model | searchSort = searchSort }, Cmd.none )

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        SearchTypeChanged searchType ->
            ( { model | searchType = searchType }, Cmd.none )

        Submit ->
            case (convertControlsToSearchCriteria model) of
                Err errors ->
                    ( { model | errors = errors }, Cmd.none )

                Ok searchCriteria ->
                    ( { model | errors = Dict.empty, searchCriteria = searchCriteria }, Cmd.none )



-- VIEW


view : Maybe Authentication -> I18n.Language -> Model -> Html Msg
view authentication language model =
    nav [ class "navbar navbar-light bg-faded" ]
        [ form [ class "form-inline", onSubmit (ForSelf Submit) ]
            [ viewInlineSearchType
                model.searchType
                (Dict.get "searchType" model.errors)
                (ForSelf << SearchTypeChanged)
            , viewInlineSearchSort
                model.searchSort
                (Dict.get "searchSort" model.errors)
                (ForSelf << SearchSortChanged)
            , viewInlineSearchLanguageCode
                model.searchLanguageCode
                (Dict.get "searchLanguageCode" model.errors)
                (ForSelf << SearchLanguageCodeChanged)
            , viewInlineSearchTerm
                language
                model.searchTerm
                (Dict.get "searchTerm" model.errors)
                (ForSelf << SearchTermChanged)
            , button [ class "btn btn-outline-success", type_ "button" ]
                [ span [ class "fa fa-search" ] []
                , text " "
                , text "Search"
                ]
            ]
        ]
