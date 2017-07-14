module Routes exposing (..)

import Assertions.Item.Routes
import Authenticator.Routes
import Cards.Item.Routes
import I18n
import Navigation
import UrlParser exposing ((</>), map, oneOf, parsePath, Parser, remaining, s, string, top)
import Values.Item.Routes


type AssertionsRoute
    = AssertionRoute String Assertions.Item.Routes.Route
    | AssertionsIndexRoute
    | NewAssertionRoute


type CardsRoute
    = CardRoute String Cards.Item.Routes.Route
    | CardsIndexRoute



-- | NewCardRoute


type LocalizedRoute
    = AboutRoute
    | AssertionsRoute AssertionsRoute
    | AuthenticatorRoute Authenticator.Routes.Route
    | CardsRoute CardsRoute
    | NotFoundRoute (List String)
    | SearchRoute
    | UserProfileRoute
    | ValuesRoute ValuesRoute


type Route
    = I18nRouteWithLanguage I18n.Language LocalizedRoute
    | I18nRouteWithoutLanguage String


type ValuesRoute
    = NewValueRoute
    | ValueRoute String Values.Item.Routes.Route
    | ValuesIndexRoute


assertionRouteParser : Parser (Assertions.Item.Routes.Route -> a) a
assertionRouteParser =
    oneOf
        [ map Assertions.Item.Routes.IndexRoute top
        ]


assertionsRouteParser : Parser (AssertionsRoute -> a) a
assertionsRouteParser =
    oneOf
        [ map AssertionsIndexRoute top
        , map NewAssertionRoute (s "new")
        , map AssertionRoute (idParser </> assertionRouteParser)
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
        , map AssertionsRoute (s "assertions" </> assertionsRouteParser)
        , map CardsRoute (s "cards" </> cardsRouteParser)
        , map UserProfileRoute (s "profile")
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
