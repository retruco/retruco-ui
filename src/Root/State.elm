module Root.State exposing (..)

import Authenticator.Routes exposing (..)
import Authenticator.State
import Card.State
import Cards.State
import Decoders
import Dom.Scroll
import Erl
import Json.Decode
import I18n
import Navigation
import NewValue.State
import Ports
import Root.Types exposing (..)
import Routes exposing (..)
import Search
import Task
import Types exposing (Flags)
import Urls
import Value.State
import Values.State


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        searchModel =
            Search.init
    in
        { authentication =
            Json.Decode.decodeValue Decoders.userForPortDecoder flags.authentication
                |> Result.toMaybe
        , authenticatorCancelMsg = Nothing
        , authenticatorCompletionMsg = Nothing
        , authenticatorModel = Authenticator.State.init
        , cardModel = Card.State.init
        , cardsModel = Cards.State.init
        , location = location
        , navigatorLanguage =
            flags.language
                |> String.left 2
                |> String.toLower
                |> I18n.languageFromIso639_1
        , newValueModel = NewValue.State.init
        , page = "reference"
        , route = Routes.I18nRouteWithoutLanguage ""
        , searchCriteria = searchModel.searchCriteria
        , searchModel = searchModel
        , signOutMsg = Nothing
        , valueModel = Value.State.init
        , valuesModel = Values.State.init
        }
            |> update (LocationChanged location)


navigate : Model -> String -> Cmd msg
navigate model path =
    -- TODO: Better handle when currentUrLPath contains a scheme, domain name, etc and not path.
    if model.location.href /= path then
        Navigation.newUrl path
    else
        Cmd.none


requireSignIn : I18n.Language -> Navigation.Location -> Model -> ( Model, Cmd Msg )
requireSignIn language location model =
    ( { model
        | authenticatorCancelMsg = model.signOutMsg
        , authenticatorCompletionMsg =
            Just <| Navigate location.href
      }
    , Navigation.modifyUrl
        (Urls.languagePath language "/sign_in")
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        -- TODO Fix duplicate messages with port "fileContentRead", that was worked around by a "ImageSelectedStatus"
        -- constructor.
        [ Sub.map NewValueMsg (NewValue.State.subscriptions model.newValueModel)
          -- , Sub.map StatementsMsg (Statements.subscriptions model.statementsModel)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        language =
            case model.route of
                I18nRouteWithLanguage language _ ->
                    language

                _ ->
                    I18n.English
    in
        case msg of
            AuthenticatorMsg childMsg ->
                let
                    ( authenticatorModel, childCmd ) =
                        Authenticator.State.update childMsg model.authenticatorModel
                in
                    ( { model | authenticatorModel = authenticatorModel }
                    , Cmd.map translateAuthenticatorMsg childCmd
                    )

            AuthenticatorTerminated route result ->
                case result of
                    Err () ->
                        ( { model
                            | authenticatorCancelMsg = Nothing
                            , authenticatorCompletionMsg = Nothing
                          }
                        , case model.authenticatorCancelMsg of
                            Just cancelMsg ->
                                Task.perform (\_ -> cancelMsg) (Task.succeed ())

                            Nothing ->
                                navigate model <| Urls.languagePath language "/"
                        )

                    Ok authentication ->
                        { model
                            | authentication = authentication
                            , authenticatorCancelMsg = Nothing
                            , authenticatorCompletionMsg = Nothing
                        }
                            ! [ Ports.storeAuthentication (Ports.userToUserForPort authentication)
                              , case model.authenticatorCompletionMsg of
                                    Just completionMsg ->
                                        Task.perform (\_ -> completionMsg) (Task.succeed ())

                                    Nothing ->
                                        navigate model
                                            (case route of
                                                ChangePasswordRoute _ ->
                                                    Urls.languagePath language "/profile"

                                                _ ->
                                                    Urls.languagePath language "/"
                                            )
                              ]

            CardMsg childMsg ->
                let
                    ( cardModel, childCmd ) =
                        Card.State.update childMsg model.cardModel
                in
                    ( { model | cardModel = cardModel }
                    , Cmd.map translateCardMsg childCmd
                    )

            CardsMsg childMsg ->
                let
                    ( cardsModel, childCmd ) =
                        Cards.State.update childMsg model.cardsModel
                in
                    ( { model | cardsModel = cardsModel }
                    , Cmd.map translateCardsMsg childCmd
                    )

            ChangeAuthenticatorRoute authenticatorRoute ->
                let
                    path =
                        case authenticatorRoute of
                            ActivateRoute userId ->
                                "/users/" ++ userId ++ "/activate"

                            ChangePasswordRoute userId ->
                                "/users/" ++ userId ++ "/reset-password"

                            ResetPasswordRoute ->
                                "/reset_password"

                            SignInRoute ->
                                "/sign_in"

                            SignOutRoute ->
                                "/sign_out"

                            SignUpRoute ->
                                "/sign_up"
                in
                    ( model
                    , navigate model <| Urls.languagePath language path
                    )

            LocationChanged location ->
                urlUpdate location model

            Navigate path ->
                ( case model.authentication of
                    Just _ ->
                        model

                    Nothing ->
                        { model | signOutMsg = Just (NavigateFromAuthenticator model.location.href) }
                , navigate model path
                )

            NavigateFromAuthenticator path ->
                ( model, navigate model path )

            NewValueMsg childMsg ->
                let
                    ( newValueModel, childCmd ) =
                        NewValue.State.update childMsg model.newValueModel
                in
                    ( { model | newValueModel = newValueModel }
                    , Cmd.map translateNewValueMsg childCmd
                    )

            NoOp ->
                ( model, Cmd.none )

            SearchMsg childMsg ->
                -- let
                --     ( searchModel, childEffect ) =
                --         Search.update childMsg model.authentication model.searchModel
                --     searchCriteria =
                --         searchModel.searchCriteria
                --     ( statementsModel, statementsEffect ) =
                --         if searchCriteria /= model.searchCriteria then
                --             Statements.update Statements.Load model.authentication searchCriteria model.statementsModel
                --         else
                --             ( model.statementsModel, Cmd.none )
                -- in
                --     { model
                --         | searchCriteria = searchCriteria
                --         , searchModel = searchModel
                --         , statementsModel = statementsModel
                --     }
                --         ! [ Cmd.map translateSearchMsg childEffect, Cmd.map translateStatementsMsg statementsEffect ]
                ( model, Cmd.none )

            ValueMsg childMsg ->
                let
                    ( valueModel, childCmd ) =
                        Value.State.update childMsg model.valueModel
                in
                    ( { model | valueModel = valueModel }
                    , Cmd.map translateValueMsg childCmd
                    )

            ValuesMsg childMsg ->
                let
                    ( valuesModel, childCmd ) =
                        Values.State.update childMsg model.valuesModel
                in
                    ( { model | valuesModel = valuesModel }
                    , Cmd.map translateValuesMsg childCmd
                    )


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        searchQuery =
            Urls.querySearchTerm location

        ( newModel, cmd ) =
            case parseLocation location of
                Just ((I18nRouteWithLanguage language localizedRoute) as route) ->
                    let
                        ( localizedModel, localizedCmd ) =
                            case localizedRoute of
                                AboutRoute ->
                                    ( model, Cmd.none )

                                AuthenticatorRoute authenticatorRoute ->
                                    let
                                        ( authenticatorModel, childCmd ) =
                                            Authenticator.State.urlUpdate
                                                language
                                                location
                                                authenticatorRoute
                                                model.authenticatorModel
                                    in
                                        ( { model
                                            | authenticatorModel = authenticatorModel
                                            , authenticatorCancelMsg =
                                                if model.authenticatorCancelMsg == Nothing then
                                                    Just (NavigateFromAuthenticator model.location.href)
                                                else
                                                    model.authenticatorCancelMsg
                                            , authenticatorCompletionMsg =
                                                if model.authenticatorCompletionMsg == Nothing then
                                                    if authenticatorRoute == SignOutRoute then
                                                        model.signOutMsg
                                                    else
                                                        Just (NavigateFromAuthenticator model.location.href)
                                                else
                                                    model.authenticatorCompletionMsg
                                          }
                                        , Cmd.map translateAuthenticatorMsg childCmd
                                        )

                                CardsRoute childRoute ->
                                    case childRoute of
                                        CardRoute cardId ->
                                            let
                                                ( cardModel, childCmd ) =
                                                    Card.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        cardId
                                                        model.cardModel
                                            in
                                                ( { model | cardModel = cardModel }
                                                , Cmd.map translateCardMsg childCmd
                                                )

                                        CardsIndexRoute ->
                                            let
                                                ( cardsModel, childCmd ) =
                                                    Cards.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        model.cardsModel
                                            in
                                                ( { model | cardsModel = cardsModel }
                                                , Cmd.map translateCardsMsg childCmd
                                                )

                                -- NewCardRoute ->
                                --     case model.authentication of
                                --         Just _ ->
                                --             let
                                --                 ( newCardModel, childCmd ) =
                                --                     NewCard.State.urlUpdate
                                --                         model.authentication
                                --                         language
                                --                         location
                                --                         model.newCardModel
                                --             in
                                --                 ( { model
                                --                     | newCardModel = newCardModel
                                --                     , signOutMsg =
                                --                         Just <|
                                --                             NavigateFromAuthenticator <|
                                --                                 Urls.languagePath language "/cards"
                                --                   }
                                --                 , Cmd.map translateNewCardMsg childCmd
                                --                 )
                                --         Nothing ->
                                --             requireSignIn language location model
                                NotFoundRoute _ ->
                                    ( model
                                    , Ports.setDocumentMetadata
                                        { description = I18n.translate language I18n.PageNotFoundDescription
                                        , imageUrl = Urls.appLogoFullUrl
                                        , title = I18n.translate language I18n.PageNotFound
                                        }
                                    )

                                SearchRoute ->
                                    -- ( model, Cmd.map translateStatementsMsg (Statements.load) )
                                    ( model, Cmd.none )

                                UserProfileRoute ->
                                    ( model, Cmd.none )

                                ValuesRoute childRoute ->
                                    case childRoute of
                                        NewValueRoute ->
                                            case model.authentication of
                                                Just _ ->
                                                    let
                                                        ( newValueModel, childCmd ) =
                                                            NewValue.State.urlUpdate
                                                                model.authentication
                                                                language
                                                                location
                                                                model.newValueModel
                                                    in
                                                        ( { model
                                                            | newValueModel = newValueModel
                                                            , signOutMsg =
                                                                Just <|
                                                                    NavigateFromAuthenticator <|
                                                                        Urls.languagePath language "/values"
                                                          }
                                                        , Cmd.map translateNewValueMsg childCmd
                                                        )

                                                Nothing ->
                                                    requireSignIn language location model

                                        ValueRoute valueId ->
                                            let
                                                ( valueModel, childCmd ) =
                                                    Value.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        valueId
                                                        model.valueModel
                                            in
                                                ( { model | valueModel = valueModel }
                                                , Cmd.map translateValueMsg childCmd
                                                )

                                        ValuesIndexRoute ->
                                            let
                                                ( valuesModel, childCmd ) =
                                                    Values.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        model.valuesModel
                                            in
                                                ( { model | valuesModel = valuesModel }
                                                , Cmd.map translateValuesMsg childCmd
                                                )
                    in
                        ( { localizedModel | route = route }, localizedCmd )

                Just ((I18nRouteWithoutLanguage path) as route) ->
                    let
                        language =
                            model.navigatorLanguage |> Maybe.withDefault I18n.English

                        command =
                            Urls.languagePath language
                                (path ++ (Urls.queryStringForParams [ "q", "tagIds" ] location))
                                |> Navigation.modifyUrl
                    in
                        ( { model | route = route }, command )

                Nothing ->
                    let
                        language =
                            model.navigatorLanguage |> Maybe.withDefault I18n.English

                        url =
                            location.href
                                |> Erl.parse

                        newUrl =
                            { url | path = (I18n.iso639_1FromLanguage language) :: url.path }
                    in
                        ( model, Navigation.modifyUrl (Erl.toString newUrl) )
    in
        { newModel | location = location }
            ! [ Task.attempt
                    (\result ->
                        case result of
                            Result.Err err ->
                                Debug.crash ("Dom.Scroll.toTop \"html-element\": " ++ toString err)

                            Result.Ok _ ->
                                NoOp
                    )
                    (Dom.Scroll.toTop "html-element")
              , cmd
              ]
