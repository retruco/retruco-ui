module Root.State exposing (..)

import About.State
import Affirmations.Index.State
import Affirmations.Item.State
import Affirmations.New.State
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
import Properties.Item.State
import Root.Types exposing (..)
import Routes exposing (..)
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
                |> I18n.languageFromLanguageId

        language =
            navigatorLanguage |> Maybe.withDefault I18n.English
    in
        { aboutModel = Nothing
        , affirmationModel = Nothing
        , affirmationsModel = Nothing
        , authentication = authentication
        , authenticatorCancelMsg = Nothing
        , authenticatorCompletionMsgs = []
        , authenticatorModel = Authenticator.State.init
        , cardModel = Nothing
        , cardsModel = Nothing
        , clearModelOnUrlUpdate = True
        , location = location
        , navigatorLanguage = navigatorLanguage
        , newAffirmationModel = Nothing
        , newValueModel = Nothing
        , propertyModel = Nothing
        , route = Routes.I18nRouteWithoutLanguage ""
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
        [ case model.affirmationModel of
            Just affirmationModel ->
                Just <| Sub.map AffirmationMsg (Affirmations.Item.State.subscriptions affirmationModel)

            Nothing ->
                Nothing
        , case model.cardModel of
            Just cardModel ->
                Just <| Sub.map CardMsg (Cards.Item.State.subscriptions cardModel)

            Nothing ->
                Nothing
        , case model.newAffirmationModel of
            Just newAffirmationModel ->
                Just <| Sub.map NewAffirmationMsg (Affirmations.New.State.subscriptions newAffirmationModel)

            Nothing ->
                Nothing
        , case model.newValueModel of
            Just newValueModel ->
                Just <| Sub.map NewValueMsg (Values.New.State.subscriptions newValueModel)

            Nothing ->
                Nothing
        , case model.propertyModel of
            Just propertyModel ->
                Just <| Sub.map PropertyMsg (Properties.Item.State.subscriptions propertyModel)

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
            AboutMsg childMsg ->
                case model.aboutModel of
                    Just aboutModel ->
                        let
                            ( updatedAboutModel, childCmd ) =
                                About.State.update childMsg aboutModel
                        in
                            ( { model | aboutModel = Just updatedAboutModel }
                            , Cmd.map translateAboutMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AffirmationMsg childMsg ->
                case model.affirmationModel of
                    Just affirmationModel ->
                        let
                            ( updatedAffirmationModel, childCmd ) =
                                Affirmations.Item.State.update childMsg affirmationModel
                        in
                            ( { model | affirmationModel = Just updatedAffirmationModel }
                            , Cmd.map translateAffirmationMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AffirmationsMsg childMsg ->
                case model.affirmationsModel of
                    Just affirmationsModel ->
                        let
                            ( updatedAffirmationsModel, childCmd ) =
                                Affirmations.Index.State.update childMsg affirmationsModel
                        in
                            ( { model | affirmationsModel = Just updatedAffirmationsModel }
                            , Cmd.map translateAffirmationsMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            AffirmationUpserted data ->
                update (Navigate <| Urls.languagePath language ("/affirmations/" ++ data.id)) model

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

            NewAffirmationMsg childMsg ->
                case model.newAffirmationModel of
                    Just newAffirmationModel ->
                        let
                            ( updatedNewAffirmationModel, childCmd ) =
                                Affirmations.New.State.update childMsg newAffirmationModel
                        in
                            ( { model | newAffirmationModel = Just updatedNewAffirmationModel }
                            , Cmd.map translateNewAffirmationMsg childCmd
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

            PropertyMsg childMsg ->
                case model.propertyModel of
                    Just propertyModel ->
                        let
                            ( updatedPropertyModel, childCmd ) =
                                Properties.Item.State.update childMsg propertyModel
                        in
                            ( { model | propertyModel = Just updatedPropertyModel }
                            , Cmd.map translatePropertyMsg childCmd
                            )

                    Nothing ->
                        ( model, Cmd.none )

            RequireSignInForAffirmation affirmationCompletionMsg ->
                requireSignInOrUpdate <| AffirmationMsg affirmationCompletionMsg

            RequireSignInForCard cardCompletionMsg ->
                requireSignInOrUpdate <| CardMsg cardCompletionMsg

            RequireSignInForNewAffirmation newAffirmationCompletionMsg ->
                requireSignInOrUpdate <| NewAffirmationMsg newAffirmationCompletionMsg

            RequireSignInForNewValue newValueCompletionMsg ->
                requireSignInOrUpdate <| NewValueMsg newValueCompletionMsg

            RequireSignInForProperty propertyCompletionMsg ->
                requireSignInOrUpdate <| PropertyMsg propertyCompletionMsg

            RequireSignInForValue valueCompletionMsg ->
                requireSignInOrUpdate <| ValueMsg valueCompletionMsg

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
                    | aboutModel = Nothing
                    , affirmationModel = Nothing
                    , affirmationsModel = Nothing
                    , cardModel = Nothing
                    , cardsModel = Nothing
                    , clearModelOnUrlUpdate = True
                    , newAffirmationModel = Nothing
                    , newValueModel = Nothing
                    , propertyModel = Nothing
                    , valueModel = Nothing
                    , valuesModel = Nothing
                }
            else
                { model
                    | clearModelOnUrlUpdate = True
                }

        ( newModel, cmd ) =
            case parseLocation location of
                Just ((I18nRouteWithLanguage language localizedRoute) as route) ->
                    let
                        ( localizedModel, localizedCmd ) =
                            case localizedRoute of
                                AboutRoute ->
                                    let
                                        aboutModel =
                                            case model.aboutModel of
                                                Just existingAboutModel ->
                                                    existingAboutModel

                                                Nothing ->
                                                    About.State.init model.authentication language

                                        ( updatedAboutModel, updatedAboutCmd ) =
                                            About.State.urlUpdate location aboutModel
                                    in
                                        ( { cleanModel | aboutModel = Just updatedAboutModel }
                                        , Cmd.map translateAboutMsg updatedAboutCmd
                                        )

                                AffirmationsRoute childRoute ->
                                    case childRoute of
                                        AffirmationRoute affirmationId affirmationRoute ->
                                            let
                                                affirmationModel =
                                                    case ( model.affirmationModel, clearSubModels ) of
                                                        ( Just affirmationModel, False ) ->
                                                            Affirmations.Item.State.setContext
                                                                model.authentication
                                                                language
                                                                affirmationModel

                                                        _ ->
                                                            Affirmations.Item.State.init
                                                                model.authentication
                                                                language
                                                                affirmationId

                                                ( updatedAffirmationModel, updatedAffirmationCmd ) =
                                                    Affirmations.Item.State.urlUpdate
                                                        location
                                                        affirmationRoute
                                                        affirmationModel
                                            in
                                                ( { cleanModel
                                                    | affirmationModel = Just updatedAffirmationModel
                                                    , -- Stay at the current location after sign out.
                                                      signOutMsg = Just (NavigateFromAuthenticator location.href)
                                                  }
                                                , Cmd.map translateAffirmationMsg updatedAffirmationCmd
                                                )

                                        AffirmationsIndexRoute ->
                                            let
                                                affirmationsModel =
                                                    Affirmations.Index.State.init model.authentication language

                                                ( updatedAffirmationsModel, childCmd ) =
                                                    Affirmations.Index.State.urlUpdate location affirmationsModel
                                            in
                                                ( { cleanModel | affirmationsModel = Just updatedAffirmationsModel }
                                                , Cmd.map translateAffirmationsMsg childCmd
                                                )

                                        NewAffirmationRoute ->
                                            let
                                                newAffirmationModel =
                                                    case ( model.newAffirmationModel, clearSubModels ) of
                                                        ( Just newAffirmationModel, False ) ->
                                                            Affirmations.New.State.setContext
                                                                model.authentication
                                                                language
                                                                newAffirmationModel

                                                        _ ->
                                                            Affirmations.New.State.init
                                                                model.authentication
                                                                language

                                                ( updatedNewAffirmationModel, updatedNewAffirmationCmd ) =
                                                    Affirmations.New.State.urlUpdate location newAffirmationModel
                                            in
                                                ( { cleanModel
                                                    | newAffirmationModel = Just updatedNewAffirmationModel
                                                    , signOutMsg =
                                                        Just <|
                                                            NavigateFromAuthenticator <|
                                                                Urls.languagePath language "/affirmations"
                                                  }
                                                , Cmd.map translateNewAffirmationMsg updatedNewAffirmationCmd
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

                                HomeRoute ->
                                    ( cleanModel, navigate cleanModel <| Urls.languagePath language "/affirmations" )

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

                                PropertiesRoute childRoute ->
                                    case childRoute of
                                        PropertyRoute propertyId propertyRoute ->
                                            let
                                                propertyModel =
                                                    case ( model.propertyModel, clearSubModels ) of
                                                        ( Just propertyModel, False ) ->
                                                            Properties.Item.State.setContext
                                                                model.authentication
                                                                language
                                                                propertyModel

                                                        _ ->
                                                            Properties.Item.State.init
                                                                model.authentication
                                                                language
                                                                propertyId

                                                ( updatedPropertyModel, updatedPropertyCmd ) =
                                                    Properties.Item.State.urlUpdate
                                                        location
                                                        propertyRoute
                                                        propertyModel
                                            in
                                                ( { cleanModel
                                                    | propertyModel = Just updatedPropertyModel
                                                    , -- Stay at the current location after sign out.
                                                      signOutMsg = Just (NavigateFromAuthenticator location.href)
                                                  }
                                                , Cmd.map translatePropertyMsg updatedPropertyCmd
                                                )

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
                            { url | path = (I18n.languageIdFromLanguage language) :: url.path }
                    in
                        ( cleanModel, Navigation.modifyUrl (Erl.toString newUrl) )
    in
        { newModel | location = location }
            ! [ Task.attempt
                    (\result ->
                        case result of
                            Err err ->
                                Debug.crash ("Dom.Scroll.toTop \"html-element\": " ++ toString err)

                            Ok _ ->
                                NoOp
                    )
                    (Dom.Scroll.toTop "html-element")
              , cmd
              ]
