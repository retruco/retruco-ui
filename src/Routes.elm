module Routes exposing (..)

import Authenticator.Routes
import Cards.Item.Routes
import Discussions.Item.Routes
import Discussions.Routes
import I18n
import Navigation
import Properties.Item.Routes
import UrlParser exposing ((</>), map, oneOf, parsePath, Parser, remaining, s, string, top)
import Values.Item.Routes


type CardsRoute
    = CardRoute String Cards.Item.Routes.Route
    | CardsIndexRoute
    | NewCardRoute


type LocalizedRoute
    = AboutRoute
    | AuthenticatorRoute Authenticator.Routes.Route
    | CardsRoute CardsRoute
    | DiscussionsRoute Discussions.Routes.Route
    | HomeRoute
    | NotFoundRoute (List String)
    | PropertiesRoute PropertiesRoute
    | ProposalsRoute ProposalsRoute
    | UserProfileRoute
    | ValuesRoute ValuesRoute


type PropertiesRoute
    = PropertyRoute String Properties.Item.Routes.Route


type ProposalsRoute
    = ProposalsIndexRoute
    | NewProposalRoute


type Route
    = I18nRouteWithLanguage Bool I18n.Language LocalizedRoute
    | I18nRouteWithoutLanguage String


type ValuesRoute
    = NewValueRoute
    | ValueRoute String Values.Item.Routes.Route
    | ValuesIndexRoute


cardRouteParser : Parser (Cards.Item.Routes.Route -> a) a
cardRouteParser =
    oneOf
        [ map Cards.Item.Routes.PropertiesRoute top
        , map Cards.Item.Routes.DebatePropertiesRoute (s "arguments")
        , map Cards.Item.Routes.DiscussionRoute (s "discussion" </> discussionRouteParser)
        , map Cards.Item.Routes.SameObjectAndKeyPropertiesRoute (s "properties" </> idParser)
        , map Cards.Item.Routes.PropertiesAsValueRoute (s "uses")
        ]


cardsRouteParser : Parser (CardsRoute -> a) a
cardsRouteParser =
    oneOf
        [ map CardsIndexRoute top
        , map NewCardRoute (s "new")
        , map CardRoute (idParser </> cardRouteParser)
        ]


discussionRouteParser : Parser (Discussions.Item.Routes.Route -> a) a
discussionRouteParser =
    oneOf
        [ map Discussions.Item.Routes.InterventionsRoute top
        , map Discussions.Item.Routes.IdeasRoute (s "ideas")
        , map Discussions.Item.Routes.QuestionsRoute (s "questions")
        ]


discussionsRouteParser : Parser (Discussions.Routes.Route -> a) a
discussionsRouteParser =
    oneOf
        [ map Discussions.Routes.DiscussionsIndexRoute top
        , map Discussions.Routes.NewDiscussionRoute (s "new")
        ]


idParser : Parser (String -> a) a
idParser =
    string


localizedRouteParser : Parser (LocalizedRoute -> a) a
localizedRouteParser =
    oneOf
        [ map HomeRoute top
        , map AboutRoute (s "about")
        , map CardsRoute (s "cards" </> cardsRouteParser)
        , map DiscussionsRoute (s "discussions" </> discussionsRouteParser)
        , map UserProfileRoute (s "profile")
        , map PropertiesRoute (s "properties" </> propertiesRouteParser)
        , map ProposalsRoute (s "proposals" </> proposalsRouteParser)
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
        [ map Properties.Item.Routes.DebatePropertiesRoute top
        , map Properties.Item.Routes.PropertiesRoute (s "properties")
        , map Properties.Item.Routes.SameObjectAndKeyPropertiesRoute (s "properties" </> idParser)
        , map Properties.Item.Routes.PropertiesAsValueRoute (s "uses")
        ]


propertiesRouteParser : Parser (PropertiesRoute -> a) a
propertiesRouteParser =
    oneOf
        [ map PropertyRoute (idParser </> propertyRouteParser)
        ]


proposalsRouteParser : Parser (ProposalsRoute -> a) a
proposalsRouteParser =
    oneOf
        [ map ProposalsIndexRoute top
        , map NewProposalRoute (s "new")
        ]


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        (List.map
            (\language ->
                map
                    (I18nRouteWithLanguage True language)
                    (s "embed" </> s (I18n.languageIdFromLanguage language) </> localizedRouteParser)
            )
            [ I18n.English
            , I18n.French
            , I18n.Spanish
            ]
            ++ List.map
                (\language ->
                    map
                        (I18nRouteWithLanguage False language)
                        (s (I18n.languageIdFromLanguage language) </> localizedRouteParser)
                )
                [ I18n.English
                , I18n.French
                , I18n.Spanish
                ]
        )


valueRouteParser : Parser (Values.Item.Routes.Route -> a) a
valueRouteParser =
    oneOf
        [ map Values.Item.Routes.DebatePropertiesRoute top
        , map Values.Item.Routes.DetailsRoute (s "details")
        , map Values.Item.Routes.PropertiesRoute (s "properties")
        , map Values.Item.Routes.SameObjectAndKeyPropertiesRoute (s "properties" </> idParser)
        , map Values.Item.Routes.PropertiesAsValueRoute (s "uses")
        ]


valuesRouteParser : Parser (ValuesRoute -> a) a
valuesRouteParser =
    oneOf
        [ map ValuesIndexRoute top
        , map NewValueRoute (s "new")
        , map ValueRoute (idParser </> valueRouteParser)
        ]
