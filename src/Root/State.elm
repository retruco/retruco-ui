module Root.State exposing (..)

import Arguments.Item.State
import Assertions.Index.State
import Assertions.Item.State
import Assertions.New.State
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
        { argumentModel = Nothing
        , assertionModel = Nothing
        , assertionsModel = Nothing
        , authentication = authentication
        , authenticatorCancelMsg = Nothing
        , authenticatorCompletionMsgs = []
        , authenticatorModel = Authenticator.State.init
        , cardModel = Nothing
        , cardsModel = Nothing
        , clearModelOnUrlUpdate = True
        , location = location
        , navigatorLanguage = navigatorLanguage
        , newAssertionModel = Nothing
        , newValueModel = Nothing
        , page = "reference"
        , route = Routes.I18nRouteWithoutLanguage ""
        , searchCriteria = searchModel.searchCriteria
        , searchModel = searchModel
        , signOutMsg = Nothing
        , valueModel = Nothing
        , valuesModel = Nothing
        }
            |> update (LocationChanged location)


navigate : Model -> String -> Cmd msg
navigate model path =
    -- TODO: Better handle when currentUrLPath contains a scheme, domain name, etc and not path.
    if model.location.href /= path then
        Navigation.newUrl path
    else
        Cmd.none


requireSignIn : I18n.Language -> Navigation.Location -> Maybe Msg -> Model -> ( Model, Cmd Msg )
requireSignIn language location completionMsg model =
    ( { model
        | authenticatorCancelMsg = model.signOutMsg
        , authenticatorCompletionMsgs =
            [ NavigateFromAuthenticator location.href ]
                ++ case completionMsg of
                    Just completionMsg ->
                        [ completionMsg ]

                    Nothing ->
                        []
      }
    , Navigation.modifyUrl
        (Urls.languagePath language "/sign_in")
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- TODO Fix duplicate messages with port "fileContentRead", that was worked around by a "ImageSelectedStatus"
    -- constructor.
    List.filterMap identity
        [ case model.argumentModel of
            Just argumentModel ->
                Just <| Sub.map ArgumentMsg (Arguments.Item.State.subscriptions argumentModel)

            Nothing ->
                Nothing
        , case model.assertionModel of
            Just assertionModel ->
                Just <| Sub.map AssertionMsg (Assertions.Item.State.subscriptions assertionModel)

            Nothing ->
                Nothing
        , case model.cardModel of
            Just cardModel ->
                Just <| Sub.map CardMsg (Cards.Item.State.subscriptions cardModel)

            Nothing ->
                Nothing
        , case model.newAssertionModel of
            Just newAssertionModel ->
                Just <| Sub.map NewAssertionMsg (Assertions.New.State.subscriptions newAssertionModel)

            Nothing ->
                Nothing
        , case model.newValueModel of
            Just newValueModel ->
                Just <| Sub.map NewValueMsg (Values.New.State.subscriptions newValueModel)

            Nothing ->
                Nothing
        , case model.valueModel of
            Just valueModel ->
                Just <| Sub.map ValueMsg (Values.Item.State.subscriptions valueModel)

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

        requireSignInOrUpdate : Msg -> ( Model, Cmd Msg )
        requireSignInOrUpdate completionMsg =
            if model.authentication == Nothing then
                -- update (StartAuthenticator Nothing (Just completionMsg) SignInRoute) model
                requireSignIn language model.location (Just completionMsg) model
            else
                update completionMsg model
    in
        case msg of
            ArgumentMsg childMsg ->
                case model.argumentModel of
                    Just argumentModel ->
                        let
                            ( updatedArgumentModel, childCmd ) =
                                Arguments.Item.State.update childMsg argumentModel
                        in
                            ( { model | argumentModel = Just updatedArgumentModel }
                            , Cmd.map translateArgumentMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AssertionMsg childMsg ->
                case model.assertionModel of
                    Just assertionModel ->
                        let
                            ( updatedAssertionModel, childCmd ) =
                                Assertions.Item.State.update childMsg assertionModel
                        in
                            ( { model | assertionModel = Just updatedAssertionModel }
                            , Cmd.map translateAssertionMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AssertionsMsg childMsg ->
                case model.assertionsModel of
                    Just assertionsModel ->
                        let
                            ( updatedAssertionsModel, childCmd ) =
                                Assertions.Index.State.update childMsg assertionsModel
                        in
                            ( { model | assertionsModel = Just updatedAssertionsModel }
                            , Cmd.map translateAssertionsMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AssertionUpserted data ->
                update (Navigate <| Urls.languagePath language ("/assertions/" ++ data.id)) model

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
                            , authenticatorCompletionMsgs = []
                            , clearModelOnUrlUpdate = False
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
                            , authenticatorCompletionMsgs = []
                            , clearModelOnUrlUpdate = False
                        }
                            ! ([ Ports.storeAuthentication authentication ]
                                ++ if List.isEmpty model.authenticatorCompletionMsgs then
                                    [ navigate model
                                        (case route of
                                            ChangePasswordRoute _ ->
                                                Urls.languagePath language "/profile"

                                            _ ->
                                                Urls.languagePath language "/"
                                        )
                                    ]
                                   else
                                    List.map
                                        (\completionMsg -> Task.perform (\_ -> completionMsg) (Task.succeed ()))
                                        model.authenticatorCompletionMsgs
                              )

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
                case model.cardsModel of
                    Just cardsModel ->
                        let
                            ( updatedCardsModel, childCmd ) =
                                Cards.Index.State.update childMsg cardsModel
                        in
                            ( { model | cardsModel = Just updatedCardsModel }
                            , Cmd.map translateCardsMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

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

            NewAssertionMsg childMsg ->
                case model.newAssertionModel of
                    Just newAssertionModel ->
                        let
                            ( updatedNewAssertionModel, childCmd ) =
                                Assertions.New.State.update childMsg newAssertionModel
                        in
                            ( { model | newAssertionModel = Just updatedNewAssertionModel }
                            , Cmd.map translateNewAssertionMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

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

            RequireSignInForArgument argumentCompletionMsg ->
                requireSignInOrUpdate <| ArgumentMsg argumentCompletionMsg

            RequireSignInForAssertion assertionCompletionMsg ->
                requireSignInOrUpdate <| AssertionMsg assertionCompletionMsg

            RequireSignInForCard cardCompletionMsg ->
                requireSignInOrUpdate <| CardMsg cardCompletionMsg

            RequireSignInForNewAssertion newAssertionCompletionMsg ->
                requireSignInOrUpdate <| NewAssertionMsg newAssertionCompletionMsg

            RequireSignInForNewValue newValueCompletionMsg ->
                requireSignInOrUpdate <| NewValueMsg newValueCompletionMsg

            RequireSignInForValue valueCompletionMsg ->
                requireSignInOrUpdate <| ValueMsg valueCompletionMsg

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
                case model.valueModel of
                    Just valueModel ->
                        let
                            ( updatedValueModel, childCmd ) =
                                Values.Item.State.update childMsg valueModel
                        in
                            ( { model | valueModel = Just updatedValueModel }
                            , Cmd.map translateValueMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            ValuesMsg childMsg ->
                case model.valuesModel of
                    Just valuesModel ->
                        let
                            ( updatedValuesModel, childCmd ) =
                                Values.Index.State.update childMsg valuesModel
                        in
                            ( { model | valuesModel = Just updatedValuesModel }
                            , Cmd.map translateValuesMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            ValueUpserted data ->
                update (Navigate <| Urls.languagePath language ("/values/" ++ data.id)) model


urlUpdate : Navigation.Location -> Model -> ( Model, Cmd Msg )
urlUpdate location model =
    let
        clearSubModels =
            model.clearModelOnUrlUpdate

        cleanModel =
            if clearSubModels then
                { model
                    | argumentModel = Nothing
                    , assertionModel = Nothing
                    , assertionsModel = Nothing
                    , cardModel = Nothing
                    , cardsModel = Nothing
                    , clearModelOnUrlUpdate = True
                    , newAssertionModel = Nothing
                    , newValueModel = Nothing
                    , valueModel = Nothing
                    , valuesModel = Nothing
                }
            else
                { model
                    | clearModelOnUrlUpdate = True
                }

        searchQuery =
            Urls.querySearchTerm location

        ( newModel, cmd ) =
            case parseLocation location of
                Just ((I18nRouteWithLanguage language localizedRoute) as route) ->
                    let
                        ( localizedModel, localizedCmd ) =
                            case localizedRoute of
                                AboutRoute ->
                                    ( cleanModel, Cmd.none )

                                ArgumentsRoute childRoute ->
                                    case childRoute of
                                        ArgumentRoute argumentId argumentRoute ->
                                            let
                                                argumentModel =
                                                    case ( model.argumentModel, clearSubModels ) of
                                                        ( Just argumentModel, False ) ->
                                                            Arguments.Item.State.setContext
                                                                model.authentication
                                                                language
                                                                argumentModel

                                                        _ ->
                                                            Arguments.Item.State.init
                                                                model.authentication
                                                                language
                                                                argumentId

                                                ( updatedArgumentModel, updatedArgumentCmd ) =
                                                    Arguments.Item.State.urlUpdate
                                                        location
                                                        argumentRoute
                                                        argumentModel
                                            in
                                                ( { cleanModel
                                                    | argumentModel = Just updatedArgumentModel
                                                    , -- Stay at the current location after sign out.
                                                      signOutMsg = Just (NavigateFromAuthenticator location.href)
                                                  }
                                                , Cmd.map translateArgumentMsg updatedArgumentCmd
                                                )

                                AssertionsRoute childRoute ->
                                    case childRoute of
                                        AssertionRoute assertionId assertionRoute ->
                                            let
                                                assertionModel =
                                                    case ( model.assertionModel, clearSubModels ) of
                                                        ( Just assertionModel, False ) ->
                                                            Assertions.Item.State.setContext
                                                                model.authentication
                                                                language
                                                                assertionModel

                                                        _ ->
                                                            Assertions.Item.State.init
                                                                model.authentication
                                                                language
                                                                assertionId

                                                ( updatedAssertionModel, updatedAssertionCmd ) =
                                                    Assertions.Item.State.urlUpdate
                                                        location
                                                        assertionRoute
                                                        assertionModel
                                            in
                                                ( { cleanModel
                                                    | assertionModel = Just updatedAssertionModel
                                                    , -- Stay at the current location after sign out.
                                                      signOutMsg = Just (NavigateFromAuthenticator location.href)
                                                  }
                                                , Cmd.map translateAssertionMsg updatedAssertionCmd
                                                )

                                        AssertionsIndexRoute ->
                                            let
                                                assertionsModel =
                                                    Assertions.Index.State.init model.authentication language

                                                ( updatedAssertionsModel, childCmd ) =
                                                    Assertions.Index.State.urlUpdate location assertionsModel
                                            in
                                                ( { cleanModel | assertionsModel = Just updatedAssertionsModel }
                                                , Cmd.map translateAssertionsMsg childCmd
                                                )

                                        NewAssertionRoute ->
                                            let
                                                newAssertionModel =
                                                    case ( model.newAssertionModel, clearSubModels ) of
                                                        ( Just newAssertionModel, False ) ->
                                                            Assertions.New.State.setContext
                                                                model.authentication
                                                                language
                                                                newAssertionModel

                                                        _ ->
                                                            Assertions.New.State.init
                                                                model.authentication
                                                                language
                                                                []

                                                ( updatedNewAssertionModel, updatedNewAssertionCmd ) =
                                                    Assertions.New.State.urlUpdate location newAssertionModel
                                            in
                                                ( { cleanModel
                                                    | newAssertionModel = Just updatedNewAssertionModel
                                                    , signOutMsg =
                                                        Just <|
                                                            NavigateFromAuthenticator <|
                                                                Urls.languagePath language "/assertions"
                                                  }
                                                , Cmd.map translateNewAssertionMsg updatedNewAssertionCmd
                                                )

                                AuthenticatorRoute authenticatorRoute ->
                                    let
                                        ( authenticatorModel, childCmd ) =
                                            Authenticator.State.urlUpdate
                                                language
                                                location
                                                authenticatorRoute
                                                model.authenticatorModel
                                    in
                                        ( { -- Don't use cleanModel instead of model, because we want to go back to
                                            -- current sub-model after authenticator process.
                                            model
                                            | authenticatorModel = authenticatorModel
                                            , authenticatorCancelMsg =
                                                if model.authenticatorCancelMsg == Nothing then
                                                    Just (NavigateFromAuthenticator model.location.href)
                                                else
                                                    model.authenticatorCancelMsg
                                            , authenticatorCompletionMsgs =
                                                if List.isEmpty model.authenticatorCompletionMsgs then
                                                    if authenticatorRoute == SignOutRoute then
                                                        case model.signOutMsg of
                                                            Just signOutMsg ->
                                                                [ signOutMsg ]

                                                            Nothing ->
                                                                []
                                                    else
                                                        [ NavigateFromAuthenticator model.location.href ]
                                                else
                                                    model.authenticatorCompletionMsgs
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
                                                ( { cleanModel | cardModel = Just updatedCardModel }
                                                , Cmd.map translateCardMsg updatedCardCmd
                                                )

                                        CardsIndexRoute ->
                                            let
                                                cardsModel =
                                                    Cards.Index.State.init model.authentication language

                                                ( updatedCardsModel, childCmd ) =
                                                    Cards.Index.State.urlUpdate
                                                        location
                                                        cardsModel
                                            in
                                                ( { cleanModel | cardsModel = Just updatedCardsModel }
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
                                --                 ( { cleanModel
                                --                     | newCardModel = newCardModel
                                --                     , signOutMsg =
                                --                         Just <|
                                --                             NavigateFromAuthenticator <|
                                --                                 Urls.languagePath language "/cards"
                                --                   }
                                --                 , Cmd.map translateNewCardMsg childCmd
                                --                 )
                                --         Nothing ->
                                --             requireSignIn language location Nothing cleanModel
                                NotFoundRoute _ ->
                                    ( cleanModel
                                    , Ports.setDocumentMetadata
                                        { description = I18n.translate language I18n.PageNotFoundDescription
                                        , imageUrl = Urls.appLogoFullUrl
                                        , title = I18n.translate language I18n.PageNotFound
                                        }
                                    )

                                SearchRoute ->
                                    -- ( cleanModel, Cmd.map translateStatementsMsg (Statements.load) )
                                    -- ( cleanModel, Cmd.none )
                                    ( cleanModel, navigate cleanModel <| Urls.languagePath language "/assertions" )

                                UserProfileRoute ->
                                    ( cleanModel, Cmd.none )

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
                                                        ( { cleanModel
                                                            | newValueModel = Just updatedNewValueModel
                                                            , signOutMsg =
                                                                Just <|
                                                                    NavigateFromAuthenticator <|
                                                                        Urls.languagePath language "/values"
                                                          }
                                                        , Cmd.map translateNewValueMsg updatedNewValueCmd
                                                        )

                                                Nothing ->
                                                    requireSignIn language location Nothing cleanModel

                                        ValueRoute valueId valueRoute ->
                                            let
                                                valueModel =
                                                    Values.Item.State.init model.authentication language valueId

                                                ( updatedValueModel, updatedValueCmd ) =
                                                    Values.Item.State.urlUpdate location valueRoute valueModel
                                            in
                                                ( { cleanModel | valueModel = Just updatedValueModel }
                                                , Cmd.map translateValueMsg updatedValueCmd
                                                )

                                        ValuesIndexRoute ->
                                            let
                                                valuesModel =
                                                    Values.Index.State.init model.authentication language

                                                ( updatedValuesModel, childCmd ) =
                                                    Values.Index.State.urlUpdate location valuesModel
                                            in
                                                ( { cleanModel | valuesModel = Just updatedValuesModel }
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
                        ( { cleanModel | route = route }, command )

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
                        ( cleanModel, Navigation.modifyUrl (Erl.toString newUrl) )
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
