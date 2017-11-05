module Root.Types exposing (..)

import About.Types
import Authenticator.Routes
import Authenticator.Types exposing (Authentication)
import Cards.Index.Types
import Cards.Item.Types
import Cards.New.Types
import I18n
import Navigation
import Properties.Item.Types
import Proposals.Index.Types
import Proposals.New.Types
import Routes
import Situations.Index.Types
import Situations.New.Types
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
    , location : Navigation.Location
    , navigatorLanguage : Maybe I18n.Language
    , newCardModel : Maybe Cards.New.Types.Model
    , newProposalModel : Maybe Proposals.New.Types.Model
    , newSituationModel : Maybe Situations.New.Types.Model
    , newValueModel : Maybe Values.New.Types.Model
    , propertyModel : Maybe Properties.Item.Types.Model
    , proposalsModel : Maybe Proposals.Index.Types.Model
    , route : Routes.Route
    , signOutMsg : Maybe Msg
    , situationsModel : Maybe Situations.Index.Types.Model
    , valueModel : Maybe Values.Item.Types.Model
    , valuesModel : Maybe Values.Index.Types.Model
    }


type Msg
    = AboutMsg About.Types.InternalMsg
    | AuthenticatorMsg Authenticator.Types.InternalMsg
    | AuthenticatorTerminated Authenticator.Routes.Route (Result () (Maybe Authentication))
    | CardMsg Cards.Item.Types.InternalMsg
    | CardsMsg Cards.Index.Types.InternalMsg
    | CardUpserted Types.DataId
    | ChangeAuthenticatorRoute Authenticator.Routes.Route
    | GraphqlInited
    | LocationChanged Navigation.Location
    | Navigate String
    | NavigateFromAuthenticator String
    | NewCardMsg Cards.New.Types.InternalMsg
    | NewProposalMsg Proposals.New.Types.InternalMsg
    | NewSituationMsg Situations.New.Types.InternalMsg
    | NewValueMsg Values.New.Types.InternalMsg
    | PropertyMsg Properties.Item.Types.InternalMsg
    | ProposalsMsg Proposals.Index.Types.InternalMsg
    | ProposalUpserted Types.DataId
    | RequireSignInForCard Cards.Item.Types.InternalMsg
    | RequireSignInForNewCard Cards.New.Types.InternalMsg
    | RequireSignInForNewProposal Proposals.New.Types.InternalMsg
    | RequireSignInForNewSituation Situations.New.Types.InternalMsg
    | RequireSignInForNewValue Values.New.Types.InternalMsg
    | RequireSignInForProperty Properties.Item.Types.InternalMsg
    | RequireSignInForValue Values.Item.Types.InternalMsg
    | ScrolledToTop
    | SituationsMsg Situations.Index.Types.InternalMsg
    | SituationUpserted Types.DataId
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


translateNewCardMsg : Cards.New.Types.MsgTranslator Msg
translateNewCardMsg =
    Cards.New.Types.translateMsg
        { onCardUpserted = CardUpserted
        , onInternalMsg = NewCardMsg
        , onRequireSignIn = RequireSignInForNewCard
        }


translateNewProposalMsg : Proposals.New.Types.MsgTranslator Msg
translateNewProposalMsg =
    Proposals.New.Types.translateMsg
        { onInternalMsg = NewProposalMsg
        , onProposalUpserted = ProposalUpserted
        , onRequireSignIn = RequireSignInForNewProposal
        }


translateNewSituationMsg : Situations.New.Types.MsgTranslator Msg
translateNewSituationMsg =
    Situations.New.Types.translateMsg
        { onInternalMsg = NewSituationMsg
        , onRequireSignIn = RequireSignInForNewSituation
        , onSituationUpserted = SituationUpserted
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


translateSituationsMsg : Situations.Index.Types.MsgTranslator Msg
translateSituationsMsg =
    Situations.Index.Types.translateMsg
        { onInternalMsg = SituationsMsg
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
