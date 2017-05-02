module Root.State exposing (..)

import Authenticator.Routes exposing (..)
import Authenticator.State
import Cards.Index.State
import Cards.Item.State
import Decoders
import Dom.Scroll
import Erl
import Json.Decode
import I18n
import Navigation
import Ports
import Root.Types exposing (..)
import Routes exposing (..)
import Search
import Task
import Types exposing (Flags)
import Urls
import Values.Index.State
import Values.Item.State
import Values.New.State


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        authentication =
            Json.Decode.decodeValue Decoders.userDecoder flags.authentication
                |> Result.toMaybe

        navigatorLanguage =
            flags.language
                |> String.left 2
                |> String.toLower
                |> I18n.languageFromIso639_1

        language =
            navigatorLanguage |> Maybe.withDefault I18n.English

        searchModel =
            Search.init
    in
        { authentication = authentication
        , authenticatorCancelMsg = Nothing
        , authenticatorCompletionMsg = Nothing
        , authenticatorModel = Authenticator.State.init
        , cardModel = Nothing
        , cardsModel = Cards.Index.State.init
        , location = location
        , navigatorLanguage = navigatorLanguage
        , newValueModel = Nothing
        , page = "reference"
        , route = Routes.I18nRouteWithoutLanguage ""
        , searchCriteria = searchModel.searchCriteria
        , searchModel = searchModel
        , signOutMsg = Nothing
        , valueModel = Values.Item.State.init
        , valuesModel = Values.Index.State.init
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
    -- TODO Fix duplicate messages with port "fileContentRead", that was worked around by a "ImageSelectedStatus"
    -- constructor.
    List.filterMap identity
        [ case model.cardModel of
            Just cardModel ->
                Just <| Sub.map CardMsg (Cards.Item.State.subscriptions cardModel)

            Nothing ->
                Nothing
        , case model.newValueModel of
            Just newValueModel ->
                Just <| Sub.map NewValueMsg (Values.New.State.subscriptions newValueModel)

            Nothing ->
                Nothing
        ]
        |> Sub.batch


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
                            ! [ Ports.storeAuthentication authentication
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
                case model.cardModel of
                    Just cardModel ->
                        let
                            ( updatedCardModel, childCmd ) =
                                Cards.Item.State.update childMsg cardModel
                        in
                            ( { model | cardModel = Just updatedCardModel }
                            , Cmd.map translateCardMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            CardsMsg childMsg ->
                let
                    ( cardsModel, childCmd ) =
                        Cards.Index.State.update childMsg model.cardsModel
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
                case model.newValueModel of
                    Just newValueModel ->
                        let
                            ( updatedNewValueModel, childCmd ) =
                                Values.New.State.update childMsg newValueModel
                        in
                            ( { model | newValueModel = Just updatedNewValueModel }
                            , Cmd.map translateNewValueMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            NoOp ->
                ( model, Cmd.none )

            RequireSignInForCard cardCompletionMsg ->
                if model.authentication == Nothing then
                    -- update (StartAuthenticator Nothing (Just (CardMsg cardCompletionMsg)) SignInRoute) model
                    requireSignIn language model.location model
                else
                    update (CardMsg cardCompletionMsg) model

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
                        Values.Item.State.update childMsg model.valueModel
                in
                    ( { model | valueModel = valueModel }
                    , Cmd.map translateValueMsg childCmd
                    )

            ValuesMsg childMsg ->
                let
                    ( valuesModel, childCmd ) =
                        Values.Index.State.update childMsg model.valuesModel
                in
                    ( { model | valuesModel = valuesModel }
                    , Cmd.map translateValuesMsg childCmd
                    )

            ValueUpserted data ->
                update (Navigate <| Urls.languagePath language ("/values/" ++ data.id)) model


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        searchQuery =
            Urls.querySearchTerm location

        unroutedModel =
            { model
                | cardModel = Nothing
                , newValueModel = Nothing
            }

        ( newModel, cmd ) =
            case parseLocation location of
                Just ((I18nRouteWithLanguage language localizedRoute) as route) ->
                    let
                        ( localizedModel, localizedCmd ) =
                            case localizedRoute of
                                AboutRoute ->
                                    ( unroutedModel, Cmd.none )

                                AuthenticatorRoute authenticatorRoute ->
                                    let
                                        ( authenticatorModel, childCmd ) =
                                            Authenticator.State.urlUpdate
                                                language
                                                location
                                                authenticatorRoute
                                                model.authenticatorModel
                                    in
                                        ( { unroutedModel
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

                                CardsRoute cardsRoute ->
                                    case cardsRoute of
                                        CardRoute cardId cardRoute ->
                                            let
                                                cardModel =
                                                    Cards.Item.State.init model.authentication language cardId

                                                ( updatedCardModel, updatedCardCmd ) =
                                                    Cards.Item.State.urlUpdate
                                                        location
                                                        cardRoute
                                                        cardModel
                                            in
                                                ( { unroutedModel | cardModel = Just updatedCardModel }
                                                , Cmd.map translateCardMsg updatedCardCmd
                                                )

                                        CardsIndexRoute ->
                                            let
                                                ( cardsModel, childCmd ) =
                                                    Cards.Index.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        model.cardsModel
                                            in
                                                ( { unroutedModel | cardsModel = cardsModel }
                                                , Cmd.map translateCardsMsg childCmd
                                                )

                                -- NewCardRoute ->
                                --     case model.authentication of
                                --         Just _ ->
                                --             let
                                --                 ( newCardModel, childCmd ) =
                                --                     NewCards.Item.State.urlUpdate
                                --                         model.authentication
                                --                         language
                                --                         location
                                --                         model.newCardModel
                                --             in
                                --                 ( { unroutedModel
                                --                     | newCardModel = newCardModel
                                --                     , signOutMsg =
                                --                         Just <|
                                --                             NavigateFromAuthenticator <|
                                --                                 Urls.languagePath language "/cards"
                                --                   }
                                --                 , Cmd.map translateNewCardMsg childCmd
                                --                 )
                                --         Nothing ->
                                --             requireSignIn language location unroutedModel
                                NotFoundRoute _ ->
                                    ( unroutedModel
                                    , Ports.setDocumentMetadata
                                        { description = I18n.translate language I18n.PageNotFoundDescription
                                        , imageUrl = Urls.appLogoFullUrl
                                        , title = I18n.translate language I18n.PageNotFound
                                        }
                                    )

                                SearchRoute ->
                                    -- ( unroutedModel, Cmd.map translateStatementsMsg (Statements.load) )
                                    ( unroutedModel, Cmd.none )

                                UserProfileRoute ->
                                    ( unroutedModel, Cmd.none )

                                ValuesRoute childRoute ->
                                    case childRoute of
                                        NewValueRoute ->
                                            case model.authentication of
                                                Just _ ->
                                                    let
                                                        newValueModel =
                                                            Values.New.State.init model.authentication language []

                                                        ( updatedNewValueModel, updatedNewValueCmd ) =
                                                            Values.New.State.urlUpdate location newValueModel
                                                    in
                                                        ( { unroutedModel
                                                            | newValueModel = Just updatedNewValueModel
                                                            , signOutMsg =
                                                                Just <|
                                                                    NavigateFromAuthenticator <|
                                                                        Urls.languagePath language "/values"
                                                          }
                                                        , Cmd.map translateNewValueMsg updatedNewValueCmd
                                                        )

                                                Nothing ->
                                                    requireSignIn language location unroutedModel

                                        ValueRoute valueId ->
                                            let
                                                ( valueModel, childCmd ) =
                                                    Values.Item.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        valueId
                                                        model.valueModel
                                            in
                                                ( { unroutedModel | valueModel = valueModel }
                                                , Cmd.map translateValueMsg childCmd
                                                )

                                        ValuesIndexRoute ->
                                            let
                                                ( valuesModel, childCmd ) =
                                                    Values.Index.State.urlUpdate
                                                        model.authentication
                                                        language
                                                        location
                                                        model.valuesModel
                                            in
                                                ( { unroutedModel | valuesModel = valuesModel }
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
                        ( { unroutedModel | route = route }, command )

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
                        ( unroutedModel, Navigation.modifyUrl (Erl.toString newUrl) )
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
