module Cards.Item.Routes exposing (..)

import Discussions.Item.Routes


type Route
    = DebatePropertiesRoute
    | DiscussionRoute Discussions.Item.Routes.Route
    | PropertiesAsValueRoute
    | PropertiesRoute
    | SameObjectAndKeyPropertiesRoute String
