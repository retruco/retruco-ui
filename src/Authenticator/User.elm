module Authenticator.User exposing (Body, decodeBody, User)


import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias Body =
    { data : User
    }


type alias User =
    { apiKey : String
    , email : String
    , name : String
    , urlName : String
    }


decodeBody : Json.Decode.Decoder Body
decodeBody =
    Json.Decode.succeed Body
        |: ("data" := decodeUser)


decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.succeed User
        |: ("apiKey" := Json.Decode.string)
        |: ("email" := Json.Decode.string)
        |: ("name" := Json.Decode.string)
        |: ("urlName" := Json.Decode.string)
