module Routes exposing (makeUrl, Route(..), urlParser)

import Authenticator.Model
import Hop
import Hop.Matchers exposing (match1, match2)
import Hop.Types
import Navigation


type Route
    = AuthenticatorRoute Authenticator.Model.Route
    -- | Component String
    | Home
    -- | ReferencePage
    | StatementsRoute


-- all : Parser String
-- all =
--     Combine.regex ".+"


-- idParser : Parser String
-- idParser =
--     Combine.regex ".+"


makeUrl : String -> String
makeUrl path = Hop.makeUrl routerConfig path 


matchers : List (Hop.Types.PathMatcher Route)
matchers =
    [ match1 Home ""
    -- , match2 Component "/reference/" all
    -- , match1 Documentation "/documentation"
    -- , match2 DocumentationPage "/documentation/" all
    -- , match1 ReferencePage "/reference"
    , match1 (AuthenticatorRoute Authenticator.Model.SignInRoute) "/sign_in"
    , match1 (AuthenticatorRoute Authenticator.Model.SignOutRoute) "/sign_out"
    , match1 (AuthenticatorRoute Authenticator.Model.SignUpRoute) "/sign_up"
    , match1 StatementsRoute "/statements"
      -- , match2 Statement "/statements/" idParser
    ]


routerConfig : Hop.Types.Config Route
routerConfig =
    -- Production:
    -- { hash = False
    -- , basePath = ""
    -- , matchers = matchers
    -- , notFound = Home
    -- }
    -- Development:
    { hash = True
    , basePath = ""
    , matchers = matchers
    , notFound = Home
    }


urlParser : Navigation.Parser ( Route, Hop.Types.Location )
urlParser =
    Navigation.makeParser (.href >> Hop.matchUrl routerConfig)
