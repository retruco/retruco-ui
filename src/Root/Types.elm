module Root.Types exposing (..)

import About.Types
import Affirmations.Index.Types
import Affirmations.Item.Types
import Affirmations.New.Types
import Authenticator.Routes
import Authenticator.Types exposing (Authentication)
import Cards.Index.Types
import Cards.Item.Types
import I18n
import Navigation
import Properties.Item.Types
import Routes
import Types
import Values.Index.Types
import Values.Item.Types
import Values.New.Types


type alias Model =
    { aboutModel : Maybe About.Types.Model
    , affirmationModel : Maybe Affirmations.Item.Types.Model
    , affirmationsModel : Maybe Affirmations.Index.Types.Model
    , authentication : Maybe Authentication
    , authenticatorCancelMsg : Maybe Msg
    , authenticatorCompletionMsgs : List Msg
    , authenticatorModel : Authenticator.Types.Model
    , cardModel : Maybe Cards.Item.Types.Model
    , cardsModel : Maybe Cards.Index.Types.Model
    , clearModelOnUrlUpdate : Bool
    , location : Navigation.Location
    , navigatorLanguage : Maybe I18n.Language
    , newAffirmationModel : Maybe Affirmations.New.Types.Model
    , newValueModel : Maybe Values.New.Types.Model
    , propertyModel : Maybe Properties.Item.Types.Model
    , route : Routes.Route
    , signOutMsg : Maybe Msg
    , valueModel : Maybe Values.Item.Types.Model
    , valuesModel : Maybe Values.Index.Types.Model
    }


type Msg
    = AboutMsg About.Types.InternalMsg
    | AffirmationMsg Affirmations.Item.Types.InternalMsg
    | AffirmationsMsg Affirmations.Index.Types.InternalMsg
    | AffirmationUpserted Types.DataId
    | AuthenticatorMsg Authenticator.Types.InternalMsg
    | AuthenticatorTerminated Authenticator.Routes.Route (Result () (Maybe Authentication))
    | CardMsg Cards.Item.Types.InternalMsg
    | CardsMsg Cards.Index.Types.InternalMsg
    | ChangeAuthenticatorRoute Authenticator.Routes.Route
    | LocationChanged Navigation.Location
    | Navigate String
    | NavigateFromAuthenticator String
    | NewAffirmationMsg Affirmations.New.Types.InternalMsg
    | NewValueMsg Values.New.Types.InternalMsg
    | NoOp
    | PropertyMsg Properties.Item.Types.InternalMsg
    | RequireSignInForAffirmation Affirmations.Item.Types.InternalMsg
    | RequireSignInForCard Cards.Item.Types.InternalMsg
    | RequireSignInForNewAffirmation Affirmations.New.Types.InternalMsg
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


translateAffirmationMsg : Affirmations.Item.Types.MsgTranslator Msg
translateAffirmationMsg =
    Affirmations.Item.Types.translateMsg
        { onInternalMsg = AffirmationMsg
        , onNavigate = Navigate
        , onRequireSignIn = RequireSignInForAffirmation
        }


translateAffirmationsMsg : Affirmations.Index.Types.MsgTranslator Msg
translateAffirmationsMsg =
    Affirmations.Index.Types.translateMsg
        { onInternalMsg = AffirmationsMsg
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


translateNewAffirmationMsg : Affirmations.New.Types.MsgTranslator Msg
translateNewAffirmationMsg =
    Affirmations.New.Types.translateMsg
        { onInternalMsg = NewAffirmationMsg
        , onAffirmationUpserted = AffirmationUpserted
        , onRequireSignIn = RequireSignInForNewAffirmation
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
