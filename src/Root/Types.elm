module Root.Types exposing (..)

import Authenticator.Routes
import Authenticator.Types exposing (Authentication)
import Cards.Index.Types
import Cards.Item.Types
import Concepts.Index.Types
import Concepts.New.Types
import I18n
import Navigation
import Routes
import Search
import Types
import Values.Index.Types
import Values.Item.Types
import Values.New.Types


type alias Model =
    { authentication : Maybe Authentication
    , authenticatorCancelMsg : Maybe Msg
    , authenticatorCompletionMsg : Maybe Msg
    , authenticatorModel : Authenticator.Types.Model
    , cardModel : Maybe Cards.Item.Types.Model
    , cardsModel : Maybe Cards.Index.Types.Model
    , conceptsModel : Maybe Concepts.Index.Types.Model
    , location : Navigation.Location
    , navigatorLanguage : Maybe I18n.Language
    , newConceptModel : Maybe Concepts.New.Types.Model
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
    = AuthenticatorMsg Authenticator.Types.InternalMsg
    | AuthenticatorTerminated Authenticator.Routes.Route (Result () (Maybe Authentication))
    | CardMsg Cards.Item.Types.InternalMsg
    | CardsMsg Cards.Index.Types.InternalMsg
    | ChangeAuthenticatorRoute Authenticator.Routes.Route
    | ConceptsMsg Concepts.Index.Types.InternalMsg
    | LocationChanged Navigation.Location
    | Navigate String
    | NavigateFromAuthenticator String
    | NewConceptMsg Concepts.New.Types.InternalMsg
    | NewValueMsg Values.New.Types.InternalMsg
    | NoOp
    | RequireSignInForCard Cards.Item.Types.InternalMsg
    | SearchMsg Search.InternalMsg
    | ValueMsg Values.Item.Types.InternalMsg
    | ValuesMsg Values.Index.Types.InternalMsg
    | ValueUpserted Types.DataId


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


translateConceptsMsg : Concepts.Index.Types.MsgTranslator Msg
translateConceptsMsg =
    Concepts.Index.Types.translateMsg
        { onInternalMsg = ConceptsMsg
        , onNavigate = Navigate
        }


translateNewConceptMsg : Concepts.New.Types.MsgTranslator Msg
translateNewConceptMsg =
    Concepts.New.Types.translateMsg
        { onInternalMsg = NewConceptMsg
        , onConceptUpserted = ValueUpserted
        }


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = NewValueMsg
        , onValueUpserted = ValueUpserted
        }


translateValueMsg : Values.Item.Types.MsgTranslator Msg
translateValueMsg =
    Values.Item.Types.translateMsg
        { onInternalMsg = ValueMsg
        , onNavigate = Navigate
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
