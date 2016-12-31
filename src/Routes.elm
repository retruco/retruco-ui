module Routes exposing (..)

import Authenticator.Routes
import I18n
import Navigation
import UrlParser exposing ((</>), map, oneOf, parsePath, Parser, remaining, s, string, top)


type CardsRoute
    = CardRoute String
    | CardsIndexRoute



-- | NewCardRoute


type ConceptsRoute
    = NewConceptRoute
    | ConceptRoute String
    | ConceptsIndexRoute


type LocalizedRoute
    = AboutRoute
    | AuthenticatorRoute Authenticator.Routes.Route
    | CardsRoute CardsRoute
      -- | ConceptsRoute ConceptsRoute
    | NotFoundRoute (List String)
    | SearchRoute
    | UserProfileRoute
    | ValuesRoute ValuesRoute


type Route
    = I18nRouteWithLanguage I18n.Language LocalizedRoute
    | I18nRouteWithoutLanguage String


type ValuesRoute
    = NewValueRoute
    | ValueRoute String
    | ValuesIndexRoute


cardsRouteParser : Parser (CardsRoute -> a) a
cardsRouteParser =
    oneOf
        [ map CardsIndexRoute top
          -- , map NewCardRoute (s "new")
        , map CardRoute idParser
        ]


conceptsRouteParser : Parser (ConceptsRoute -> a) a
conceptsRouteParser =
    oneOf
        [ map ConceptsIndexRoute top
        , map NewConceptRoute (s "new")
        , map ConceptRoute idParser
        ]


idParser : Parser (String -> a) a
idParser =
    string


localizedRouteParser : Parser (LocalizedRoute -> a) a
localizedRouteParser =
    oneOf
        [ map SearchRoute top
        , map AboutRoute (s "about")
        , map CardsRoute (s "cards" </> cardsRouteParser)
          -- , map ConceptsRoute (s "concepts" </> conceptsRouteParser)
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


valuesRouteParser : Parser (ValuesRoute -> a) a
valuesRouteParser =
    oneOf
        [ map ValuesIndexRoute top
        , map NewValueRoute (s "new")
        , map ValueRoute idParser
        ]
