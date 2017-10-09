module Root.Types exposing (..)

import About.Types
import Authenticator.Routes
import Authenticator.Types exposing (Authentication)
import Cards.Index.Types
import Cards.Item.Types
import Debates.Index.Types
import Debates.New.Types
import I18n
import Navigation
import Properties.Item.Types
import Proposals.Index.Types
import Proposals.New.Types
import Routes
import Types
import Values.Index.Types
import Values.Item.Types
import Values.New.Types


type alias Model =
    { aboutModel : Maybe About.Types.Model
    , authentication : Maybe Authentication
    , authenticatorCancelMsg : Maybe Msg
    , authenticatorCompletionMsgs : List Msg
    , authenticatorModel : Authenticator.Types.Model
    , cardModel : Maybe Cards.Item.Types.Model
    , cardsModel : Maybe Cards.Index.Types.Model
    , clearModelOnUrlUpdate : Bool
    , debatesModel : Maybe Debates.Index.Types.Model
    , location : Navigation.Location
    , navigatorLanguage : Maybe I18n.Language
    , newDebateModel : Maybe Debates.New.Types.Model
    , newProposalModel : Maybe Proposals.New.Types.Model
    , newValueModel : Maybe Values.New.Types.Model
    , propertyModel : Maybe Properties.Item.Types.Model
    , proposalsModel : Maybe Proposals.Index.Types.Model
    , route : Routes.Route
    , signOutMsg : Maybe Msg
    , valueModel : Maybe Values.Item.Types.Model
    , valuesModel : Maybe Values.Index.Types.Model
    }


type Msg
    = AboutMsg About.Types.InternalMsg
    | AuthenticatorMsg Authenticator.Types.InternalMsg
    | AuthenticatorTerminated Authenticator.Routes.Route (Result () (Maybe Authentication))
    | CardMsg Cards.Item.Types.InternalMsg
    | CardsMsg Cards.Index.Types.InternalMsg
    | ChangeAuthenticatorRoute Authenticator.Routes.Route
    | DebatesMsg Debates.Index.Types.InternalMsg
    | DebateUpserted Types.DataId
    | LocationChanged Navigation.Location
    | Navigate String
    | NavigateFromAuthenticator String
    | NewDebateMsg Debates.New.Types.InternalMsg
    | NewProposalMsg Proposals.New.Types.InternalMsg
    | NewValueMsg Values.New.Types.InternalMsg
    | NoOp
    | PropertyMsg Properties.Item.Types.InternalMsg
    | ProposalsMsg Proposals.Index.Types.InternalMsg
    | ProposalUpserted Types.DataId
    | RequireSignInForCard Cards.Item.Types.InternalMsg
    | RequireSignInForNewDebate Debates.New.Types.InternalMsg
    | RequireSignInForNewProposal Proposals.New.Types.InternalMsg
    | RequireSignInForNewValue Values.New.Types.InternalMsg
    | RequireSignInForProperty Properties.Item.Types.InternalMsg
    | RequireSignInForValue Values.Item.Types.InternalMsg
    | ValueMsg Values.Item.Types.InternalMsg
    | ValuesMsg Values.Index.Types.InternalMsg
    | ValueUpserted Types.DataId


translateAboutMsg : About.Types.MsgTranslator Msg
translateAboutMsg =
    About.Types.translateMsg
        { onInternalMsg = AboutMsg
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


translateDebatesMsg : Debates.Index.Types.MsgTranslator Msg
translateDebatesMsg =
    Debates.Index.Types.translateMsg
        { onInternalMsg = DebatesMsg
        , onNavigate = Navigate
        }


translateNewDebateMsg : Debates.New.Types.MsgTranslator Msg
translateNewDebateMsg =
    Debates.New.Types.translateMsg
        { onInternalMsg = NewDebateMsg
        , onDebateUpserted = DebateUpserted
        , onRequireSignIn = RequireSignInForNewDebate
        }


translateNewProposalMsg : Proposals.New.Types.MsgTranslator Msg
translateNewProposalMsg =
    Proposals.New.Types.translateMsg
        { onInternalMsg = NewProposalMsg
        , onProposalUpserted = ProposalUpserted
        , onRequireSignIn = RequireSignInForNewProposal
        }


translateNewValueMsg : Values.New.Types.MsgTranslator Msg
translateNewValueMsg =
    Values.New.Types.translateMsg
        { onInternalMsg = NewValueMsg
        , onRequireSignIn = RequireSignInForNewValue
        , onValueUpserted = ValueUpserted
        }


translatePropertyMsg : Properties.Item.Types.MsgTranslator Msg
translatePropertyMsg =
    Properties.Item.Types.translateMsg
        { onInternalMsg = PropertyMsg
        , onNavigate = Navigate
        , onRequireSignIn = RequireSignInForProperty
        }


translateProposalsMsg : Proposals.Index.Types.MsgTranslator Msg
translateProposalsMsg =
    Proposals.Index.Types.translateMsg
        { onInternalMsg = ProposalsMsg
        , onNavigate = Navigate
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
