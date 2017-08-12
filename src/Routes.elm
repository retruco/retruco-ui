module Routes exposing (..)

import Affirmations.Item.Routes
import Arguments.Item.Routes
import Authenticator.Routes
import Cards.Item.Routes
import I18n
import Navigation
import Properties.Item.Routes
import UrlParser exposing ((</>), map, oneOf, parsePath, Parser, remaining, s, string, top)
import Values.Item.Routes


type AffirmationsRoute
    = AffirmationRoute String Affirmations.Item.Routes.Route
    | AffirmationsIndexRoute
    | NewAffirmationRoute


type ArgumentsRoute
    = ArgumentRoute String Arguments.Item.Routes.Route


type CardsRoute
    = CardRoute String Cards.Item.Routes.Route
    | CardsIndexRoute



-- | NewCardRoute


type LocalizedRoute
    = AboutRoute
    | AffirmationsRoute AffirmationsRoute
    | ArgumentsRoute ArgumentsRoute
    | AuthenticatorRoute Authenticator.Routes.Route
    | CardsRoute CardsRoute
    | NotFoundRoute (List String)
    | PropertiesRoute PropertiesRoute
    | SearchRoute
    | UserProfileRoute
    | ValuesRoute ValuesRoute


type PropertiesRoute
    = PropertyRoute String Properties.Item.Routes.Route


type Route
    = I18nRouteWithLanguage I18n.Language LocalizedRoute
    | I18nRouteWithoutLanguage String


type ValuesRoute
    = NewValueRoute
    | ValueRoute String Values.Item.Routes.Route
    | ValuesIndexRoute


affirmationRouteParser : Parser (Affirmations.Item.Routes.Route -> a) a
affirmationRouteParser =
    oneOf
        [ map Affirmations.Item.Routes.IndexRoute top
        ]


affirmationsRouteParser : Parser (AffirmationsRoute -> a) a
affirmationsRouteParser =
    oneOf
        [ map AffirmationsIndexRoute top
        , map NewAffirmationRoute (s "new")
        , map AffirmationRoute (idParser </> affirmationRouteParser)
        ]


argumentRouteParser : Parser (Arguments.Item.Routes.Route -> a) a
argumentRouteParser =
    oneOf
        [ map Arguments.Item.Routes.IndexRoute top
        ]


argumentsRouteParser : Parser (ArgumentsRoute -> a) a
argumentsRouteParser =
    oneOf
        [ map ArgumentRoute (idParser </> argumentRouteParser)
        ]


cardRouteParser : Parser (Cards.Item.Routes.Route -> a) a
cardRouteParser =
    oneOf
        [ map Cards.Item.Routes.IndexRoute top
        , map Cards.Item.Routes.ArgumentsRoute (s "arguments")
        , map Cards.Item.Routes.SameKeyPropertiesRoute (s "properties" </> idParser)
        ]


cardsRouteParser : Parser (CardsRoute -> a) a
cardsRouteParser =
    oneOf
        [ map CardsIndexRoute top

        -- , map NewCardRoute (s "new")
        , map CardRoute (idParser </> cardRouteParser)
        ]


idParser : Parser (String -> a) a
idParser =
    string


localizedRouteParser : Parser (LocalizedRoute -> a) a
localizedRouteParser =
    oneOf
        [ map SearchRoute top
        , map AboutRoute (s "about")
        , map AffirmationsRoute (s "affirmations" </> affirmationsRouteParser)
        , map ArgumentsRoute (s "arguments" </> argumentsRouteParser)
        , map CardsRoute (s "cards" </> cardsRouteParser)
        , map UserProfileRoute (s "profile")
        , map PropertiesRoute (s "properties" </> propertiesRouteParser)
        , map (AuthenticatorRoute Authenticator.Routes.ResetPasswordRoute) (s "reset_password")
        , map (AuthenticatorRoute Authenticator.Routes.SignInRoute) (s "sign_in")
        , map (AuthenticatorRoute Authenticator.Routes.SignOutRoute) (s "sign_out")
        , map (AuthenticatorRoute Authenticator.Routes.SignUpRoute) (s "sign_up")
        , map
            (AuthenticatorRoute << Authenticator.Routes.ActivateRoute)
            (s "users" </> idParser </> s "activate")
        , map
            (AuthenticatorRoute << Authenticator.Routes.ChangePasswordRoute)
            (s "users" </> idParser </> s "reset-password")
        , map ValuesRoute (s "values" </> valuesRouteParser)
        , map NotFoundRoute remaining
        ]


parseLocation : Navigation.Location -> Maybe Route
parseLocation location =
    UrlParser.parsePath routeParser location


propertyRouteParser : Parser (Properties.Item.Routes.Route -> a) a
propertyRouteParser =
    oneOf
        [ map Properties.Item.Routes.IndexRoute top
        ]


propertiesRouteParser : Parser (PropertiesRoute -> a) a
propertiesRouteParser =
    oneOf
        [ map PropertyRoute (idParser </> propertyRouteParser)
        ]


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        (List.map
            (\language ->
                map (I18nRouteWithLanguage language) (s (I18n.iso639_1FromLanguage language) </> localizedRouteParser)
            )
            [ I18n.English
            , I18n.French
            , I18n.Spanish
            ]
        )


valueRouteParser : Parser (Values.Item.Routes.Route -> a) a
valueRouteParser =
    oneOf
        [ map Values.Item.Routes.IndexRoute top
        , map Values.Item.Routes.ArgumentsRoute (s "arguments")

        -- , map Values.Item.Routes.SameKeyPropertiesRoute (s "properties" </> idParser)
        ]


valuesRouteParser : Parser (ValuesRoute -> a) a
valuesRouteParser =
    oneOf
        [ map ValuesIndexRoute top
        , map NewValueRoute (s "new")
        , map ValueRoute (idParser </> valueRouteParser)
        ]
