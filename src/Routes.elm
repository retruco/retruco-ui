module Routes exposing (..)

import Authenticator.Routes
import I18n
import Navigation
import UrlParser exposing ((</>), map, oneOf, parsePath, Parser, remaining, s, string, top)


type LocalizedRoute
    = AboutRoute
    | AuthenticatorRoute Authenticator.Routes.Route
    | NotFoundRoute (List String)
    | SearchRoute
    | StatementsRoute StatementsRoute
    | UserProfileRoute
    | ValuesRoute ValuesRoute


type Route
    = I18nRouteWithLanguage I18n.Language LocalizedRoute
    | I18nRouteWithoutLanguage String


type StatementsRoute
    = StatementRoute String
    | StatementsIndexRoute


type ValuesRoute
    = NewValueRoute
    | ValueRoute String
    | ValuesIndexRoute


idParser : Parser (String -> a) a
idParser =
    string


localizedRouteParser : Parser (LocalizedRoute -> a) a
localizedRouteParser =
    oneOf
        [ map SearchRoute top
        , map AboutRoute (s "about")
        , map UserProfileRoute (s "profile")
        , map (AuthenticatorRoute Authenticator.Routes.ResetPasswordRoute) (s "reset_password")
        , map (AuthenticatorRoute Authenticator.Routes.SignInRoute) (s "sign_in")
        , map (AuthenticatorRoute Authenticator.Routes.SignOutRoute) (s "sign_out")
        , map (AuthenticatorRoute Authenticator.Routes.SignUpRoute) (s "sign_up")
        , map StatementsRoute (s "statements" </> statementsRouteParser)
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


statementsRouteParser : Parser (StatementsRoute -> a) a
statementsRouteParser =
    oneOf
        [ map StatementsIndexRoute top
        , map StatementRoute idParser
        ]


valuesRouteParser : Parser (ValuesRoute -> a) a
valuesRouteParser =
    oneOf
        [ map ValuesIndexRoute top
        , map NewValueRoute (s "new")
        , map ValueRoute idParser
        ]
