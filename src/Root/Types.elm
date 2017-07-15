module Root.Types exposing (..)

import Assertions.Index.Types
import Assertions.Item.Types
import Assertions.New.Types
import Authenticator.Routes
import Authenticator.Types exposing (Authentication)
import Cards.Index.Types
import Cards.Item.Types
import I18n
import Navigation
import Routes
import Search
import Types
import Values.Index.Types
import Values.Item.Types
import Values.New.Types


type alias Model =
    { assertionModel : Maybe Assertions.Item.Types.Model
    , assertionsModel : Maybe Assertions.Index.Types.Model
    , authentication : Maybe Authentication
    , authenticatorCancelMsg : Maybe Msg
    , authenticatorCompletionMsgs : List Msg
    , authenticatorModel : Authenticator.Types.Model
    , cardModel : Maybe Cards.Item.Types.Model
    , cardsModel : Maybe Cards.Index.Types.Model
    , clearModelOnUrlUpdate : Bool
    , location : Navigation.Location
    , navigatorLanguage : Maybe I18n.Language
    , newAssertionModel : Maybe Assertions.New.Types.Model
    , newValueModel : Maybe Values.New.Types.Model
    , page : String
    , route : Routes.Route
    , searchCriteria : Types.SearchCriteria
    , searchModel : Search.Model
    , signOutMsg : Maybe Msg
    , valueModel : Maybe Values.Item.Types.Model
    , valuesModel : Maybe Values.Index.Types.Model
    }


type Msg
    = AssertionMsg Assertions.Item.Types.InternalMsg
    | AssertionsMsg Assertions.Index.Types.InternalMsg
    | AssertionUpserted Types.DataId
    | AuthenticatorMsg Authenticator.Types.InternalMsg
    | AuthenticatorTerminated Authenticator.Routes.Route (Result () (Maybe Authentication))
    | CardMsg Cards.Item.Types.InternalMsg
    | CardsMsg Cards.Index.Types.InternalMsg
    | ChangeAuthenticatorRoute Authenticator.Routes.Route
    | LocationChanged Navigation.Location
    | Navigate String
    | NavigateFromAuthenticator String
    | NewAssertionMsg Assertions.New.Types.InternalMsg
    | NewValueMsg Values.New.Types.InternalMsg
    | NoOp
    | RequireSignInForAssertion Assertions.Item.Types.InternalMsg
    | RequireSignInForCard Cards.Item.Types.InternalMsg
    | RequireSignInForNewAssertion Assertions.New.Types.InternalMsg
    | RequireSignInForNewValue Values.New.Types.InternalMsg
    | RequireSignInForValue Values.Item.Types.InternalMsg
    | SearchMsg Search.InternalMsg
    | ValueMsg Values.Item.Types.InternalMsg
    | ValuesMsg Values.Index.Types.InternalMsg
    | ValueUpserted Types.DataId


translateAssertionMsg : Assertions.Item.Types.MsgTranslator Msg
translateAssertionMsg =
    Assertions.Item.Types.translateMsg
        { onInternalMsg = AssertionMsg
        , onNavigate = Navigate
        , onRequireSignIn = RequireSignInForAssertion
        }


translateAssertionsMsg : Assertions.Index.Types.MsgTranslator Msg
translateAssertionsMsg =
    Assertions.Index.Types.translateMsg
        { onInternalMsg = AssertionsMsg
        , onNavigate = Navigate
        }


translateAuthenticatorMsg : Authenticator.Types.MsgTranslator Msg
translateAuthenticatorMsg =
    Authenticator.Types.translateMsg
        { onChangeRoute = ChangeAuthenticatorRoute
        , onInternalMsg = AuthenticatorMsg
        , onNavigate = NavigateFromAuthenticator
        , onTerminated = AuthenticatorTerminated
        }


translateCardMsg : Cards.Item.Types.MsgTranslator Msg
translateCardMsg =
    Cards.Item.Types.translateMsg
        { onInternalMsg = CardMsg
        , onNavigate = Navigate
        , onRequireSignIn = RequireSignInForCard
        }


translateCardsMsg : Cards.Index.Types.MsgTranslator Msg
translateCardsMsg =
    Cards.Index.Types.translateMsg
        { onInternalMsg = CardsMsg
        , onNavigate = Navigate
        }


translateNewAssertionMsg : Assertions.New.Types.MsgTranslator Msg
translateNewAssertionMsg =
    Assertions.New.Types.translateMsg
        { onInternalMsg = NewAssertionMsg
        , onAssertionUpserted = AssertionUpserted
        , onRequireSignIn = RequireSignInForNewAssertion
        }


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = NewValueMsg
        , onRequireSignIn = RequireSignInForNewValue
        , onValueUpserted = ValueUpserted
        }


translateValueMsg : Values.Item.Types.MsgTranslator Msg
translateValueMsg =
    Values.Item.Types.translateMsg
        { onInternalMsg = ValueMsg
        , onNavigate = Navigate
        , onRequireSignIn = RequireSignInForValue
        }


translateValuesMsg : Values.Index.Types.MsgTranslator Msg
translateValuesMsg =
    Values.Index.Types.translateMsg
        { onInternalMsg = ValuesMsg
        , onNavigate = Navigate
        }


translateSearchMsg : Search.MsgTranslator Msg
translateSearchMsg =
    Search.translateMsg
        { onInternalMsg = SearchMsg
        , onNavigate = Navigate
        }
