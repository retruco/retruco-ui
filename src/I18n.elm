module I18n exposing (..)

import Constants exposing (nameKeyIds)
import Dict exposing (Dict)
import String
import Types exposing (..)


-- STRINGS TO TRANSLATE
-- for translators who want to internationalize the application


type TranslationId
    = About
    | AboutCredits
    | AboutCreditsContent
    | AboutDescription
    | AboutLead
    | AboutLegal
    | AboutLegalContent
    | Abstain
    | AccountCreationFailed
    | ActivationDescription
    | ActivationFailed
    | ActivationInProgress
    | ActivationNotRequested
    | ActivationSucceeded
    | ActivationTitle
    | Actor GrammaticalNumber
    | Add
    | AddNew
    | AddNewItemBox
    | AddPropertyKey
    | Affirmations
    | AffirmationsDescription
    | AffirmationsRetrievalFailed
    | Agree
    | Arguments
    | ArgumentsAbout
    | ArgumentsRetrievalFailed
    | ArgumentType
    | AuthenticationFailed
    | AuthenticationRequired
    | AuthenticationRequiredExplanation
    | BadAuthorization
    | BadEmailOrPassword
    | BadPayload
    | BadPayloadExplanation
    | BadStatus
    | BadUrl
    | BadUrlExplanation
    | BestOf Int
    | BijectiveCardReference
    | Boolean
    | BooleanField
    | Cancel
    | Card
    | CardId
    | CardIdArray
    | CardIdField
    | CardPlaceholder
    | CardRetrievalFailed
    | Cards
    | CardsDescription
    | ChangePassword
    | ChangePasswordDescription
    | ChangePasswordExplanation
    | ChangePasswordTitle
    | Close
    | Colon
    | Copyright
    | CountArguments Int
    | CountVersionsAvailable Int
    | CountVotes Int
    | Create
    | CreateAccountNow
    | CreateYourAccount
    | Debate
    | DebateArgumentAgainst
    | DebateArgumentFor
    | DebateConsLabel
    | DebateProsLabel
    | Disagree
    | Edit
    | Email
    | EmailPlaceholder
    | EmailSentForAccountActivation String
    | EmptyString
    | EnterBoolean
    | EnterCard
    | EnterEmail
    | EnterImage
    | EnterNumber
    | EnterPassword
    | EnterPropertyKey
    | EnterUrl
    | EnterUsername
    | EnterValue
    | EveryLanguage
    | FalseWord
    | FindAnotherCard
    | FindAnotherPropertyKey
    | FindAnotherValue
    | FindCard
    | FindPropertyKey
    | FindValue
    | FooterAbout
    | FooterDiscover
    | GenericError
    | GiveNegativeRating
    | GivePositiveRating
    | HeaderTitle
    | Help
    | Home
    | HomeDescription
    | HomeTitle
    | Image
    | ImageAlt
    | ImageField
    | ImageUploadError String
    | ImproveExistingContent
    | InputEmailField
    | InputNumberField
    | InputUrlField
    | InvalidNumber
    | Language Language
    | LanguageWord
    | License
    | LoadingMenu
    | LocalizedString
    | MissingArguments
    | MissingDescription
    | MissingValue
    | NetworkError
    | NetworkErrorExplanation
    | New
    | NewAffirmation
    | NewAffirmationDescription
    | NewArgument
    | NewArgumentDescription
    | NewCard
    | NewValue
    | NewValueDescription
    | Number
    | NumberPlaceholder
    | Objects
    | ObjectsDescription
    | PageLoading
    | PageLoadingExplanation
    | PageNotFound
    | PageNotFoundDescription
    | PageNotFoundExplanation
    | Password
    | PasswordChangeFailed
    | PasswordLost
    | PasswordPlaceholder
    | Properties
    | PropertyKeyPlaceholder
    | RadioButtonForFollowingAutocompleter
    | ReadingSelectedImage
    | ReadMore
    | Register
    | RegisterNow
    | ResetPassword
    | ResetPasswordDescription
    | ResetPasswordExplanation
    | ResetPasswordLink
    | ResetPasswordTitle
    | RetrucoLogo
    | SameKeyPropertiesRetrievalFailed
    | Save
    | Score
    | Search
    | SearchPlaceholder
    | SelectCardOrTypeMoreCharacters
    | SelectPropertyKeyOrTypeMoreCharacters
    | SelectValueOrTypeMoreCharacters
    | Send
    | SendEmailAgain
    | Share
    | ShowAll Int
    | SignIn
    | SignInDescription
    | SignInTitle
    | SignInToContribute
    | SignOut
    | SignOutAndContributeLater
    | SignOutDescription
    | SignOutTitle
    | SignUp
    | SignUpDescription
    | SignUpTitle
    | String
    | Suggestions
    | Tags
    | TextField
    | Timeout
    | TimeoutExplanation
    | Trash
    | TrueWord
    | TweetMessage String String
    | Type
    | UnknownLanguage
    | UnknownSchemaId String
    | UnknownUser
    | UnknownValue
    | UntitledCard
    | UploadImage
    | UploadingImage String
    | Url
    | UrlPlaceholder
    | Username
    | UsernameOrEmailAlreadyExist
    | UsernamePlaceholder
    | UserProfileDescription
    | Uses
    | Value
    | ValueCreationFailed
    | ValueId
    | ValueIdArray
    | ValueIdField
    | ValuePlaceholder
    | ValueRetrievalFailed
    | Values
    | ValuesDescription
    | ValueType
    | VoteBestContributions
    | Website


emptyTranslationSet : TranslationSet
emptyTranslationSet =
    { english = todo
    , french = todo
    , spanish = todo
    }


getTranslationSet : TranslationId -> TranslationSet
getTranslationSet translationId =
    case translationId of
        About ->
            { emptyTranslationSet
                | english = s "About"
                , french = s "À propos"
                , spanish = todo
            }

        AboutCredits ->
            { emptyTranslationSet
                | english = s "Credits"
                , french = s "Crédits"
                , spanish = s "Créditos"
            }

        AboutCreditsContent ->
            { emptyTranslationSet
                | english = s "The bubble tags navigation system is based on "
                , french = s "Le système de navigations des tag par bulles est basé sur la solution "
                , spanish = todo
            }

        AboutDescription ->
            getTranslationSet AboutLead

        AboutLead ->
            { emptyTranslationSet
                | english = s "About the OGP Toolbox"
                , french = s "À propos de la boite à outils OGP"
                , spanish = todo
            }

        AboutLegal ->
            { emptyTranslationSet
                | english = s "Legal notices"
                , french = s "Mentions légales"
                , spanish = s "Nota legal"
            }

        AboutLegalContent ->
            { emptyTranslationSet
                | english = s "OGPtoolobox.org is edited by the Etalab taskforce, a Prime Minister service, 39 quai André Citroën 75015 PARIS."
                , french = s "OGPtoolobox.org est édité par la mission Etalab, service du Premier Ministre, 39 quai André Citroën 75015 PARIS."
                , spanish = todo
            }

        Abstain ->
            { emptyTranslationSet
                | english = s "Abstain"
                , french = s "M'abstenir"
                , spanish = todo
            }

        AccountCreationFailed ->
            { emptyTranslationSet
                | english = s "Account création failed"
                , french = s "Échec de la création du compte"
                , spanish = todo
            }

        ActivationDescription ->
            { emptyTranslationSet
                | english = s "Verification of the user's email address"
                , french = s "Vérification de l'adresse courriel de l'utilisateur"
                , spanish = todo
            }

        ActivationFailed ->
            { emptyTranslationSet
                | english = s "Email address verfication failed"
                , french = s "Échec de la vérification de l'adresse courriel"
                , spanish = todo
            }

        ActivationInProgress ->
            { emptyTranslationSet
                | english = s "Verifying your email address..."
                , french = s "Vérification de votre adresse courriel..."
                , spanish = todo
            }

        ActivationNotRequested ->
            { emptyTranslationSet
                | english = s "Your email address will be verified shortly..."
                , french = s "Votre adresse courriel va bientôt être vérifiée..."
                , spanish = todo
            }

        ActivationSucceeded ->
            { emptyTranslationSet
                | english = s "The verification of your email address has succeeded. Your account is now activated!"
                , french = s "La vérification de votre adresse courriel a réussi. Votre compte est maintenant activé !"
                , spanish = todo
            }

        ActivationTitle ->
            { emptyTranslationSet
                | english = s "Account Activation"
                , french = s "Activation du compte"
                , spanish = todo
            }

        Actor number ->
            { emptyTranslationSet
                | english =
                    case number of
                        Singular ->
                            s "Actor"

                        Plural ->
                            s "Actors"
                , french =
                    case number of
                        Singular ->
                            s "Acteur"

                        Plural ->
                            s "Acteurs"
                , spanish =
                    case number of
                        Singular ->
                            todo

                        Plural ->
                            todo
            }

        Add ->
            { emptyTranslationSet
                | english = s "Add"
                , french = s "Ajouter"
                , spanish = todo
            }

        AddNew ->
            { emptyTranslationSet
                | english = s "Add new"
                , french = s "Ajouter"
                , spanish = todo
            }

        AddNewItemBox ->
            { emptyTranslationSet
                | english = s "Add a new item"
                , french = s "Ajouter un nouvel élément"
                , spanish = todo
            }

        AddPropertyKey ->
            { emptyTranslationSet
                | english = s "Add property"
                , french = s "Ajouter une propriété"
                , spanish = todo
            }

        Affirmations ->
            { emptyTranslationSet
                | english = s "Affirmations"
                , french = s "Affirmations"
                , spanish = todo
            }

        AffirmationsDescription ->
            { emptyTranslationSet
                | english = s "List of affirmations"
                , french = s "Liste d'affirmations"
                , spanish = todo
            }

        AffirmationsRetrievalFailed ->
            { emptyTranslationSet
                | english = s "Retrieval of affirmations failed"
                , french = s "Échec de la récupération des affirmations"
                , spanish = todo
            }

        Agree ->
            { emptyTranslationSet
                | english = s "Agree"
                , french = s "Approuver"
                , spanish = todo
            }

        Arguments ->
            { emptyTranslationSet
                | english = s "Arguments"
                , french = s "Arguments"
                , spanish = todo
            }

        ArgumentsAbout ->
            { emptyTranslationSet
                | english = s "Arguments about"
                , french = s "Arguments sur"
                , spanish = todo
            }

        ArgumentsRetrievalFailed ->
            { emptyTranslationSet
                | english = s "Retrieval of arguments failed"
                , french = s "Échec de la récupération des arguments"
                , spanish = todo
            }

        ArgumentType ->
            { emptyTranslationSet
                | english = s "Argument Type"
                , french = s "Type d'argument"
                , spanish = todo
            }

        AuthenticationFailed ->
            { emptyTranslationSet
                | english = s "Authentication failed"
                , french = s "L'authentification a échoué"
                , spanish = todo
            }

        AuthenticationRequired ->
            { emptyTranslationSet
                | english = s "Authentication required"
                , french = todo
                , spanish = todo
            }

        AuthenticationRequiredExplanation ->
            { emptyTranslationSet
                | english = s "You must sign in to display this page."
                , french = todo
                , spanish = todo
            }

        BadAuthorization ->
            { emptyTranslationSet
                | english = s "Authorization code is wrong or obsolete."
                , french = s "Le code d'autorisation est erroné ou périmé."
                , spanish = todo
            }

        BadEmailOrPassword ->
            { emptyTranslationSet
                | english = s "Either email address is unknown or password is wrong."
                , french = s "Soit l'adresse courriel est inconnue, soit le mot de passe est erroné."
                , spanish = todo
            }

        BadPayload ->
            { emptyTranslationSet
                | english = s "Bad payload"
                , french = s "Contenu incorrect"
                , spanish = todo
            }

        BadPayloadExplanation ->
            { emptyTranslationSet
                | english = s "The server returned unexpected data."
                , french = s "Le server a retourné des données imprévues"
                , spanish = todo
            }

        BadStatus ->
            { emptyTranslationSet
                | english = s "Bad status"
                , french = s "Statut incorrect"
                , spanish = todo
            }

        BadUrl ->
            { emptyTranslationSet
                | english = s "Bad URL"
                , french = s "URL incorrecte"
                , spanish = todo
            }

        BadUrlExplanation ->
            { emptyTranslationSet
                | english = s "The given URL is invalid."
                , french = s "L'URL fournie n'est pas valide."
                , spanish = todo
            }

        BestOf count ->
            { emptyTranslationSet
                | english = s ("Best of " ++ (toString count))
                , french = s ("Meilleur parmi " ++ (toString count))
                , spanish = todo
            }

        BijectiveCardReference ->
            { emptyTranslationSet
                | english = s "Bijective link to a card"
                , french = s "Lien bijectif vers une fiche"
                , spanish = todo
            }

        Boolean ->
            { emptyTranslationSet
                | english = s "Boolean"
                , french = s "Booléen"
                , spanish = todo
            }

        BooleanField ->
            getTranslationSet Boolean

        Cancel ->
            { emptyTranslationSet
                | english = s "Cancel"
                , french = s "Annuler"
                , spanish = todo
            }

        Card ->
            { emptyTranslationSet
                | english = s "Card"
                , french = s "Fiche"
                , spanish = todo
            }

        CardId ->
            { emptyTranslationSet
                | english = s "Link to a card"
                , french = s "Lien vers une fiche"
                , spanish = todo
            }

        CardIdArray ->
            { emptyTranslationSet
                | english = s "Array of links to cards"
                , french = s "Tableau de liens vers des fiches"
                , spanish = todo
            }

        CardIdField ->
            getTranslationSet CardId

        CardPlaceholder ->
            { emptyTranslationSet
                | english = s "Name or ID of a card"
                , french = s "Nom ou identifiant d'une fiche"
                , spanish = todo
            }

        CardRetrievalFailed ->
            { emptyTranslationSet
                | english = s "Card retrieval failed"
                , french = s "Échec de la récupération de la fiche"
                , spanish = todo
            }

        Cards ->
            { emptyTranslationSet
                | english = s "Cards"
                , french = s "Fiches"
                , spanish = todo
            }

        CardsDescription ->
            { emptyTranslationSet
                | english = s "List of cards"
                , french = s "Liste de fiches"
                , spanish = todo
            }

        ChangePassword ->
            { emptyTranslationSet
                | english = s "Change your password"
                , french = s "Changez votre mot de passe"
                , spanish = todo
            }

        ChangePasswordDescription ->
            { emptyTranslationSet
                | english = s "Change of the user's password"
                , french = s "Changement du mot de passe de l'utilisateur"
                , spanish = todo
            }

        ChangePasswordExplanation ->
            { emptyTranslationSet
                | english = s "Enter a new password to be able to sign-in."
                , french = s "Entrez un nouveau mot de passe qui vous servira à vous identifier."
                , spanish = todo
            }

        ChangePasswordTitle ->
            { emptyTranslationSet
                | english = s "Password Change"
                , french = s "Changement du mot de passe"
                , spanish = todo
            }

        Close ->
            { emptyTranslationSet
                | english = s "Close"
                , french = s "Fermer"
                , spanish = todo
            }

        Colon ->
            { emptyTranslationSet
                | english = s ": "
                , french = s " : "
                , spanish = s ": "
            }

        Copyright ->
            { emptyTranslationSet
                | english = s "© 2016 Etalab. Design by Nodesign.net"
                , french = s "© 2016 Etalab. Design par Nodesign.net"
                , spanish = todo
            }

        CountArguments count ->
            { emptyTranslationSet
                | english =
                    case count of
                        0 ->
                            s "No argument"

                        1 ->
                            s "1 argument"

                        _ ->
                            s ((toString count) ++ " arguments")
                , french =
                    case count of
                        0 ->
                            s "Pas d'argument"

                        1 ->
                            s "1 argument"

                        _ ->
                            s ((toString count) ++ " arguments")
                , spanish = todo
            }

        CountVersionsAvailable count ->
            { emptyTranslationSet
                | english =
                    case count of
                        0 ->
                            s "No version available"

                        1 ->
                            s "1 version available"

                        _ ->
                            s ((toString count) ++ " versions available")
                , french =
                    case count of
                        0 ->
                            s "Aucune version disponible"

                        1 ->
                            s "1 version disponible"

                        _ ->
                            s ((toString count) ++ " versions disponibles")
                , spanish = todo
            }

        CountVotes count ->
            { emptyTranslationSet
                | english =
                    case count of
                        0 ->
                            s "0 vote"

                        1 ->
                            s "1 vote"

                        _ ->
                            s ((toString count) ++ " votes")
                , french =
                    case count of
                        0 ->
                            s "0 vote"

                        1 ->
                            s "1 vote"

                        _ ->
                            s ((toString count) ++ " votes")
                , spanish = todo
            }

        Create ->
            { emptyTranslationSet
                | english = s "Create"
                , french = s "Créer"
                , spanish = todo
            }

        CreateAccountNow ->
            { emptyTranslationSet
                | english = s "Create your account now"
                , french = s "Créez votre compte maintenant"
                , spanish = todo
            }

        CreateYourAccount ->
            { emptyTranslationSet
                | english = s "Create your account"
                , french = s "Créez votre compte"
                , spanish = todo
            }

        Debate ->
            { emptyTranslationSet
                | english = s "Debate"
                , french = s "Débattre"
                , spanish = todo
            }

        DebateArgumentAgainst ->
            { emptyTranslationSet
                | english = s "Argument Against"
                , french = s "Argument contre"
                , spanish = todo
            }

        DebateArgumentFor ->
            { emptyTranslationSet
                | english = s "Argument For"
                , french = s "Argument pour"
                , spanish = todo
            }

        DebateConsLabel ->
            { emptyTranslationSet
                | english = s "- Argument Against"
                , french = s "- Argument contre"
                , spanish = todo
            }

        DebateProsLabel ->
            { emptyTranslationSet
                | english = s "+ Argument For"
                , french = s "+ Argument pour"
                , spanish = todo
            }

        Disagree ->
            { emptyTranslationSet
                | english = s "Disagree"
                , french = s "Désapprouver"
                , spanish = todo
            }

        Edit ->
            { emptyTranslationSet
                | english = s "Edit"
                , french = s "Éditer"
                , spanish = todo
            }

        Email ->
            { emptyTranslationSet
                | english = s "Email"
                , french = s "Courriel"
                , spanish = todo
            }

        EmailPlaceholder ->
            { emptyTranslationSet
                | english = s "john.doe@example.com"
                , french = s "martine.dupont@exemple.fr"
                , spanish = todo
            }

        EmailSentForAccountActivation email ->
            { emptyTranslationSet
                | english =
                    s
                        ("An email has been sent to "
                            ++ email
                            ++ ". Click the link it contains, to activate your account."
                        )
                , french =
                    s
                        ("Un courriel a été envoyé à "
                            ++ email
                            ++ ". Cliquez sur le lien qu'il contient pour activer votre compte."
                        )
                , spanish = todo
            }

        EmptyString ->
            { emptyTranslationSet
                | english = s ""
                , french = s ""
                , spanish = s ""
            }

        EnterBoolean ->
            { emptyTranslationSet
                | english = s "Please check or uncheck the box"
                , french = s "Veuillez cocher ou décocher la case"
                , spanish = todo
            }

        EnterCard ->
            { emptyTranslationSet
                | english = s "Please enter the name or the ID of a card"
                , french = s "Veuillez entrer le nom ou l'identifiant d'une fiche"
                , spanish = todo
            }

        EnterEmail ->
            { emptyTranslationSet
                | english = s "Please enter your email"
                , french = s "Veuillez entrer votre courriel"
                , spanish = todo
            }

        EnterImage ->
            { emptyTranslationSet
                | english = s "Please select an image"
                , french = s "Veuillez sélectionner une image"
                , spanish = todo
            }

        EnterNumber ->
            { emptyTranslationSet
                | english = s "Please enter a number"
                , french = s "Veuillez entrer un nombre"
                , spanish = todo
            }

        EnterPassword ->
            { emptyTranslationSet
                | english = s "Please enter your password"
                , french = s "Veuillez entrer votre mot de passe"
                , spanish = todo
            }

        EnterPropertyKey ->
            { emptyTranslationSet
                | english = s "Please enter the name or the ID of a property"
                , french = s "Veuillez entrer le nom ou l'identifiant d'une propriété"
                , spanish = todo
            }

        EnterUrl ->
            { emptyTranslationSet
                | english = s "Please enter a link (an URL)"
                , french = s "Veuillez entrer un lien (une URL)"
                , spanish = todo
            }

        EnterUsername ->
            { emptyTranslationSet
                | english = s "Please enter your username"
                , french = s "Veuillez entrer votre nom d'utilisateur"
                , spanish = todo
            }

        EnterValue ->
            { emptyTranslationSet
                | english = s "Please enter value"
                , french = s "Veuillez entrer une valeur"
                , spanish = todo
            }

        EveryLanguage ->
            { emptyTranslationSet
                | english = s "Every language"
                , french = s "Toutes les langues"
                , spanish = todo
            }

        FalseWord ->
            { emptyTranslationSet
                | english = s "False"
                , french = s "Faux"
                , spanish = todo
            }

        FindAnotherCard ->
            { emptyTranslationSet
                | english = s "Find another card"
                , french = s "Rechercher une autre fiche"
                , spanish = todo
            }

        FindAnotherPropertyKey ->
            { emptyTranslationSet
                | english = s "Find another property"
                , french = s "Rechercher une autre propriété"
                , spanish = todo
            }

        FindAnotherValue ->
            { emptyTranslationSet
                | english = s "Find another value"
                , french = s "Rechercher une autre valeur"
                , spanish = todo
            }

        FindCard ->
            { emptyTranslationSet
                | english = s "Find a card"
                , french = s "Rechercher une fiche"
                , spanish = todo
            }

        FindPropertyKey ->
            { emptyTranslationSet
                | english = s "Find a property"
                , french = s "Rechercher une propriété"
                , spanish = todo
            }

        FindValue ->
            { emptyTranslationSet
                | english = s "Find a value"
                , french = s "Rechercher une valeur"
                , spanish = todo
            }

        FooterAbout ->
            { emptyTranslationSet
                | english = s "About"
                , french = s "À propos"
                , spanish = s "Acerca"
            }

        FooterDiscover ->
            { emptyTranslationSet
                | english = s "Discover"
                , french = s "Découvrir"
                , spanish = s "Descubrir"
            }

        GenericError ->
            { emptyTranslationSet
                | english = s "Something wrong happened!"
                , french = s "Quelque chose s'est mal passé !"
                , spanish = todo
            }

        GiveNegativeRating ->
            { emptyTranslationSet
                | english = s "Give a negative rating"
                , french = s "Donner une note négative"
                , spanish = todo
            }

        GivePositiveRating ->
            { emptyTranslationSet
                | english = s "Give a positive rating"
                , french = s "Donner une note positive"
                , spanish = todo
            }

        HeaderTitle ->
            { emptyTranslationSet
                | english = s "digital solutions to improve democracy"
                , french = s "solutions numériques pour la démocratie"
                , spanish = todo
            }

        Help ->
            { emptyTranslationSet
                | english = s "Help"
                , french = s "Aide"
                , spanish = s "Ayuda"
            }

        Home ->
            { emptyTranslationSet
                | english = s "Home"
                , french = s "Accueil"
                , spanish = s "Inicio"
            }

        HomeDescription ->
            { emptyTranslationSet
                | english = s "Digital solutions to improve democracy"
                , french = s "Solutions numériques pour la démocratie"
                , spanish = todo
            }

        HomeTitle ->
            { emptyTranslationSet
                | english = s "OGP Toolbox"
                , french = s "OGP Toolbox"
                , spanish = todo
            }

        Image ->
            { emptyTranslationSet
                | english = s "Image"
                , french = s "Image"
                , spanish = todo
            }

        ImageAlt ->
            { emptyTranslationSet
                | english = s "The uploaded image"
                , french = s "L'image téléversée"
                , spanish = todo
            }

        ImageField ->
            getTranslationSet Image

        ImageUploadError message ->
            { emptyTranslationSet
                | english = s ("Image upload error: " ++ message)
                , french = s ("Échec du téléversement de l'image :" ++ message)
                , spanish = todo
            }

        ImproveExistingContent ->
            { emptyTranslationSet
                | english = s "Improve existing content"
                , french = s "Améliorez le contenu existant"
                , spanish = todo
            }

        InputEmailField ->
            getTranslationSet Email

        InputNumberField ->
            getTranslationSet Number

        InputUrlField ->
            getTranslationSet Url

        InvalidNumber ->
            { emptyTranslationSet
                | english = s "Not a valid number"
                , french = s "Ce n'est pas un nombre valide."
                , spanish = todo
            }

        Language language ->
            case language of
                English ->
                    { emptyTranslationSet
                        | english = s "English"
                        , french = s "Anglais"
                        , spanish = s "Inglés"
                    }

                French ->
                    { emptyTranslationSet
                        | english = s "French"
                        , french = s "Français"
                        , spanish = s "Francés"
                    }

                Spanish ->
                    { emptyTranslationSet
                        | english = s "Spanish"
                        , french = s "Espagnol"
                        , spanish = s "Español"
                    }

        LanguageWord ->
            { emptyTranslationSet
                | english = s "Language"
                , french = s "Langue"
                , spanish = s "Idioma"
            }

        License ->
            { emptyTranslationSet
                | english = s "License"
                , french = s "Licence"
                , spanish = todo
            }

        LoadingMenu ->
            { emptyTranslationSet
                | english = s "Loading menu..."
                , french = s "Chargement du menu..."
                , spanish = todo
            }

        LocalizedString ->
            { emptyTranslationSet
                | english = s "Localized string"
                , french = s "Chaîne de caractères localisée"
                , spanish = todo
            }

        MissingArguments ->
            { emptyTranslationSet
                | english = s "No arguments. Let's be the first to express your opinion!"
                , french = s "Aucun argument. Soyez le premier à donner votre avis !"
                , spanish = todo
            }

        MissingDescription ->
            { emptyTranslationSet
                | english = s "Missing description"
                , french = s "Description manquante"
                , spanish = todo
            }

        MissingValue ->
            { emptyTranslationSet
                | english = s "Missing value"
                , french = s "Valeur manquante"
                , spanish = todo
            }

        NetworkError ->
            { emptyTranslationSet
                | english = s "Network error"
                , french = s "Erreur réseau"
                , spanish = todo
            }

        NetworkErrorExplanation ->
            { emptyTranslationSet
                | english = s "There was a network error."
                , french = todo
                , spanish = todo
            }

        New ->
            { emptyTranslationSet
                | english = s "New"
                , french = s "Nouveau"
                , spanish = todo
            }

        NewAffirmation ->
            { emptyTranslationSet
                | english = s "New Affirmation"
                , french = s "Nouvelle affirmation"
                , spanish = todo
            }

        NewAffirmationDescription ->
            { emptyTranslationSet
                | english = s "Form to enter a new affirmation"
                , french = s "Formulaire de création d'une nouvelle affirmation"
                , spanish = todo
            }

        NewArgument ->
            { emptyTranslationSet
                | english = s "New Argument"
                , french = s "Nouvel argument"
                , spanish = todo
            }

        NewArgumentDescription ->
            { emptyTranslationSet
                | english = s "Form to enter a new argument"
                , french = s "Formulaire de création d'un nouvel argument"
                , spanish = todo
            }

        NewCard ->
            { emptyTranslationSet
                | english = s "New Card"
                , french = s "Nouvelle fiche"
                , spanish = todo
            }

        NewValue ->
            { emptyTranslationSet
                | english = s "New Value"
                , french = s "Nouvelle valeur"
                , spanish = todo
            }

        NewValueDescription ->
            { emptyTranslationSet
                | english = s "Form to enter a new value"
                , french = s "Formulaire de création d'une nouvelle valeur"
                , spanish = todo
            }

        Number ->
            { emptyTranslationSet
                | english = s "Number"
                , french = s "Nombre"
                , spanish = todo
            }

        NumberPlaceholder ->
            { emptyTranslationSet
                | english = s "3.1415927"
                , french = s "3.1415927"
                , spanish = s "3.1415927"
            }

        Objects ->
            { emptyTranslationSet
                | english = s "Objects"
                , french = s "Objets"
                , spanish = todo
            }

        ObjectsDescription ->
            { emptyTranslationSet
                | english = s "List of objects"
                , french = s "Liste des objets"
                , spanish = todo
            }

        PageLoading ->
            { emptyTranslationSet
                | english = s "Page is loading"
                , french = s "Chargement en cours"
                , spanish = todo
            }

        PageLoadingExplanation ->
            { emptyTranslationSet
                | english = s "Data is loading and should be displayed quite soon."
                , french = todo
                , spanish = todo
            }

        PageNotFound ->
            { emptyTranslationSet
                | english = s "Page Not Found"
                , french = s "Page non trouvée"
                , spanish = todo
            }

        PageNotFoundDescription ->
            { emptyTranslationSet
                | english = s "The request pas doesn't exist."
                , french = s "La page demandée n'existe pas."
                , spanish = todo
            }

        PageNotFoundExplanation ->
            { emptyTranslationSet
                | english = s "Sorry, but the page you were trying to view does not exist."
                , french = s "Désolé mais la page que vous avez demandé n'est pas disponible"
                , spanish = todo
            }

        Password ->
            { emptyTranslationSet
                | english = s "Password"
                , french = s "Mot de passe"
                , spanish = todo
            }

        PasswordChangeFailed ->
            { emptyTranslationSet
                | english = s "Password change failed"
                , french = s "Échec du changement de mot de passe"
                , spanish = todo
            }

        PasswordLost ->
            { emptyTranslationSet
                | english = s "Password lost?"
                , french = s "Mot de passe oublié ?"
                , spanish = todo
            }

        PasswordPlaceholder ->
            { emptyTranslationSet
                | english = s "Your secret password"
                , french = s "Votre mot de passe secret"
                , spanish = todo
            }

        Properties ->
            { emptyTranslationSet
                | english = s "Properties"
                , french = s "Propriétés"
                , spanish = todo
            }

        PropertyKeyPlaceholder ->
            { emptyTranslationSet
                | english = s "Name of a property"
                , french = s "Nom d'une propriété"
                , spanish = todo
            }

        RadioButtonForFollowingAutocompleter ->
            { emptyTranslationSet
                | english = s "Radio button for following autocompleter"
                , french = s "Bouton radio de l'autocompleter suivant"
                , spanish = todo
            }

        ReadingSelectedImage ->
            { emptyTranslationSet
                | english = s "Reading selected image..."
                , french = s "Lecture de l'image sélectionnée..."
                , spanish = todo
            }

        ReadMore ->
            { emptyTranslationSet
                | english = s "Read more"
                , french = s "En savoir plus"
                , spanish = todo
            }

        Register ->
            { emptyTranslationSet
                | english = s "Register"
                , french = s "Créer le compte"
                , spanish = todo
            }

        RegisterNow ->
            { emptyTranslationSet
                | english = s "Register now!"
                , french = s "Inscrivez vous maintenant !"
                , spanish = todo
            }

        ResetPassword ->
            { emptyTranslationSet
                | english = s "Reset Password"
                , french = s "Changer de mot de passe"
                , spanish = todo
            }

        ResetPasswordDescription ->
            { emptyTranslationSet
                | english = s "Reset of the user's password"
                , french = s "Réinitialisation du mot de passe de l'utilisateur"
                , spanish = todo
            }

        ResetPasswordExplanation ->
            { emptyTranslationSet
                | english = s "Enter your email. We will send you the instructions to create a new password."
                , french = s "Entrez votre courriel. Nous vous enverrons les instructions pour changer de mot de passe."
                , spanish = todo
            }

        ResetPasswordLink ->
            { emptyTranslationSet
                | english = s "I forgot my password"
                , french = s "J'ai oublié mon mot de passe"
                , spanish = todo
            }

        ResetPasswordTitle ->
            { emptyTranslationSet
                | english = s "Password Reset"
                , french = s "Réinitialisation du mot de passe"
                , spanish = todo
            }

        RetrucoLogo ->
            { emptyTranslationSet
                | english = s "Retruco logo"
                , french = s "Logo de Retruco"
                , spanish = todo
            }

        SameKeyPropertiesRetrievalFailed ->
            { emptyTranslationSet
                | english = s "Retrieval of properties having both the samek object and the same key failed"
                , french = s "Échec de la récupération des propriétés ayant le même objet et la même clé"
                , spanish = todo
            }

        Save ->
            { emptyTranslationSet
                | english = s "Save"
                , french = s "Enregistrer"
                , spanish = todo
            }

        Score ->
            { emptyTranslationSet
                | english = s "Score"
                , french = s "Score"
                , spanish = s "Score"
            }

        Search ->
            { emptyTranslationSet
                | english = s "Search"
                , french = s "Rechercher"
                , spanish = todo
            }

        SearchPlaceholder ->
            { emptyTranslationSet
                | english = s "Search terms"
                , french = s "Termes de la recherche"
                , spanish = todo
            }

        SelectCardOrTypeMoreCharacters ->
            { emptyTranslationSet
                | english = s "Select a card or type more characters"
                , french = s "Sélectionner une fiche ou tapez plus de caractères"
                , spanish = todo
            }

        SelectPropertyKeyOrTypeMoreCharacters ->
            { emptyTranslationSet
                | english = s "Select a property or type more characters"
                , french = s "Sélectionner une propriété ou taper plus de caractères"
                , spanish = todo
            }

        SelectValueOrTypeMoreCharacters ->
            { emptyTranslationSet
                | english = s "Select a value or type more characters"
                , french = s "Sélectionner une valeur ou tapez plus de caractères"
                , spanish = todo
            }

        Send ->
            { emptyTranslationSet
                | english = s "Send"
                , french = s "Envoyer"
                , spanish = todo
            }

        SendEmailAgain ->
            { emptyTranslationSet
                | english = s "Send email again"
                , french = s "Réenvoyer le courriel"
                , spanish = todo
            }

        Share ->
            { emptyTranslationSet
                | english = s "Share"
                , french = s "Partager"
                , spanish = todo
            }

        ShowAll count ->
            { emptyTranslationSet
                | english = s ("Show all " ++ (toString count))
                , french = s ("Voir tous (" ++ (toString count) ++ ")")
                , spanish = s ("Ver todo (" ++ (toString count) ++ ")")
            }

        SignIn ->
            { emptyTranslationSet
                | english = s "Sign In"
                , french = s "Connexion"
                , spanish = s "Acceder"
            }

        SignInDescription ->
            { emptyTranslationSet
                | english = s "User's sign in"
                , french = s "Identification de l'utilisateur"
                , spanish = todo
            }

        SignInTitle ->
            { emptyTranslationSet
                | english = s "Sign In"
                , french = s "Connexion"
                , spanish = todo
            }

        SignInToContribute ->
            { emptyTranslationSet
                | english = s "Sign in to contribute"
                , french = s "Identifiez-vous pour contribuer"
                , spanish = todo
            }

        SignOut ->
            { emptyTranslationSet
                | english = s "Sign Out"
                , french = s "Déconnexion"
                , spanish = s "Salir"
            }

        SignOutAndContributeLater ->
            { emptyTranslationSet
                | english = s "Sign out and contribute later"
                , french = s "Déconnectez-vous et contribuez plus tard"
                , spanish = todo
            }

        SignOutDescription ->
            { emptyTranslationSet
                | english = s "User's sign out"
                , french = s "Déconnexion de l'utilisateur"
                , spanish = todo
            }

        SignOutTitle ->
            { emptyTranslationSet
                | english = s "Sign Out"
                , french = s "Déconnexion"
                , spanish = todo
            }

        SignUp ->
            { emptyTranslationSet
                | english = s "Sign Up"
                , french = s "Inscription"
                , spanish = s "Registrarse"
            }

        SignUpDescription ->
            { emptyTranslationSet
                | english = s "User's sign up"
                , french = s "Inscription de l'utilisateur"
                , spanish = todo
            }

        SignUpTitle ->
            { emptyTranslationSet
                | english = s "Sign Up"
                , french = s "Inscription"
                , spanish = todo
            }

        String ->
            { emptyTranslationSet
                | english = s "String"
                , french = s "Chaîne de caractères"
                , spanish = todo
            }

        Suggestions ->
            { emptyTranslationSet
                | english = s "Suggestions"
                , french = s "Suggestions"
                , spanish = todo
            }

        Tags ->
            { emptyTranslationSet
                | english = s "Tags"
                , french = s "Tags"
                , spanish = s "Tags"
            }

        TextField ->
            { emptyTranslationSet
                | english = s "Text"
                , french = s "Texte"
                , spanish = todo
            }

        Timeout ->
            { emptyTranslationSet
                | english = s "Timeout"
                , french = s "Délai dépassé"
                , spanish = todo
            }

        TimeoutExplanation ->
            { emptyTranslationSet
                | english = s "The server was too slow to respond."
                , french = s "Le serveur a mis trop de temps à repondre."
                , spanish = todo
            }

        Trash ->
            { emptyTranslationSet
                | english = s "Trash"
                , french = s "Jeter"
                , spanish = todo
            }

        TrueWord ->
            { emptyTranslationSet
                | english = s "True"
                , french = s "Vrai"
                , spanish = todo
            }

        TweetMessage name url ->
            { emptyTranslationSet
                | english = s ("Discover " ++ name ++ " on Retruco.org: " ++ url)
                , french = s ("Découvrez " ++ name ++ " dans Retruco.org : " ++ url)
                , spanish = todo
            }

        Type ->
            { emptyTranslationSet
                | english = s "Type"
                , french = s "Type"
                , spanish = s "Tipo"
            }

        UnknownLanguage ->
            { emptyTranslationSet
                | english = s "Unknown language"
                , french = s "Langue inconnue"
                , spanish = todo
            }

        UnknownSchemaId schemaId ->
            { emptyTranslationSet
                | english = s ("Reference to an unknown schema: " ++ schemaId)
                , french = s ("Référence à un schema inconnu : " ++ schemaId)
                , spanish = todo
            }

        UnknownUser ->
            { emptyTranslationSet
                | english = s "User is unknown."
                , french = s "L'utilisateur est inconnu."
                , spanish = todo
            }

        UnknownValue ->
            { emptyTranslationSet
                | english = s "Unknown value"
                , french = s "Valeur inconnue"
                , spanish = todo
            }

        UntitledCard ->
            { emptyTranslationSet
                | english = s "Untitled Card"
                , french = s "Fiche sans titre"
                , spanish = s "Tipo"
            }

        UploadImage ->
            { emptyTranslationSet
                | english = s "Upload image"
                , french = s "Téléverser une image"
                , spanish = todo
            }

        UploadingImage filename ->
            { emptyTranslationSet
                | english = s ("Uploading image \"" ++ filename ++ "\"...")
                , french = s ("Téléversement de l'image \"" ++ filename ++ "\"...")
                , spanish = todo
            }

        Url ->
            { emptyTranslationSet
                | english = s "Link (URL)"
                , french = s "Lien (URL)"
                , spanish = todo
            }

        UrlPlaceholder ->
            { emptyTranslationSet
                | english = s "https://www.example.com/sample-page"
                , french = s "https://www.exemple.fr/exemple-de-page"
                , spanish = todo
            }

        Username ->
            { emptyTranslationSet
                | english = s "Username"
                , french = s "Nom d'utilisateur"
                , spanish = todo
            }

        UsernameOrEmailAlreadyExist ->
            { emptyTranslationSet
                | english = s "Username or email are already used."
                , french = s "Le nom d'utilisateur ou le mot de passe sont déjà utilisés."
                , spanish = todo
            }

        UsernamePlaceholder ->
            { emptyTranslationSet
                | english = s "John Doe"
                , french = s "Françoise Martin"
                , spanish = todo
            }

        UserProfileDescription ->
            { emptyTranslationSet
                | english = s "The profile of user"
                , french = s "Le profil de l'utilisation"
                , spanish = todo
            }

        Uses ->
            { emptyTranslationSet
                | english = s "Uses"
                , french = s "Utilise"
                , spanish = todo
            }

        Value ->
            { emptyTranslationSet
                | english = s "Value"
                , french = s "Valeur"
                , spanish = todo
            }

        ValueCreationFailed ->
            { emptyTranslationSet
                | english = s "Value creation failed"
                , french = s "Échec de la création de la valeur"
                , spanish = todo
            }

        ValueId ->
            { emptyTranslationSet
                | english = s "Link to a value"
                , french = s "Lien vers une valeur"
                , spanish = todo
            }

        ValueIdArray ->
            { emptyTranslationSet
                | english = s "Array of links to values"
                , french = s "Tableau de liens vers des valeurs"
                , spanish = todo
            }

        ValueIdField ->
            getTranslationSet ValueId

        ValuePlaceholder ->
            { emptyTranslationSet
                | english = s "The value..."
                , french = s "La valeur"
                , spanish = todo
            }

        ValueRetrievalFailed ->
            { emptyTranslationSet
                | english = s "Value retrieval failed"
                , french = s "Échec de la récupération de la valeur"
                , spanish = todo
            }

        Values ->
            { emptyTranslationSet
                | english = s "Values"
                , french = s "Valeurs"
                , spanish = todo
            }

        ValuesDescription ->
            { emptyTranslationSet
                | english = s "List of values"
                , french = s "Liste de valeurs"
                , spanish = todo
            }

        ValueType ->
            { emptyTranslationSet
                | english = s "Value Type"
                , french = s "Type de valeur"
                , spanish = todo
            }

        VoteBestContributions ->
            { emptyTranslationSet
                | english = s "Vote for the best contributions"
                , french = s "Votez pour les meilleurs contributions"
                , spanish = todo
            }

        Website ->
            { emptyTranslationSet
                | english = s "Website"
                , french = s "Site web"
                , spanish = todo
            }



-- INTERNALS


type Language
    = English
    | French
    | Spanish


type alias TranslationSet =
    { english : Maybe String
    , french : Maybe String
    , spanish : Maybe String
    }



{-
   This type is opinionated: it satifies only the needs of this application.
   See also: https://en.wikipedia.org/wiki/Grammatical_number
-}


type GrammaticalNumber
    = Singular
    | Plural


languages : List Language
languages =
    [ English
    , French
    , Spanish
    ]


s : a -> Maybe a
s =
    Just


todo : Maybe a
todo =
    Nothing



-- FUNCTIONS


getManyStrings : Language -> List String -> Card -> Dict String TypedValue -> List String
getManyStrings language keyIds card values =
    let
        getStrings : ValueType -> List String
        getStrings value =
            case value of
                BijectiveCardReferenceValue _ ->
                    []

                BooleanValue _ ->
                    []

                CardIdArrayValue ids ->
                    []

                CardIdValue cardId ->
                    []

                EmailValue value ->
                    [ value ]

                ImagePathValue path ->
                    []

                LocalizedStringValue valueByLanguage ->
                    case getValueByPreferredLanguage language valueByLanguage of
                        Nothing ->
                            []

                        Just value ->
                            [ value ]

                NumberValue _ ->
                    []

                StringValue value ->
                    [ value ]

                UrlValue value ->
                    [ value ]

                ValueIdArrayValue ids ->
                    List.concatMap (\id -> getStrings (ValueIdValue id)) ids

                ValueIdValue valueId ->
                    case Dict.get valueId values of
                        Nothing ->
                            []

                        Just subValue ->
                            getStrings subValue.value

                WrongValue _ _ ->
                    []
    in
        keyIds
            |> List.map
                (\keyId ->
                    Dict.get keyId card.properties
                        |> Maybe.andThen (\valueId -> Dict.get valueId values)
                        |> Maybe.map (\value -> getStrings value.value)
                        |> Maybe.withDefault []
                )
            |> List.filter (not << List.isEmpty)
            |> List.head
            |> Maybe.withDefault []


getOneString : Language -> List String -> Card -> Dict String TypedValue -> Maybe String
getOneString language keyIds card values =
    keyIds
        |> List.map
            (\keyId ->
                Dict.get keyId card.properties
                    |> Maybe.andThen (\valueId -> Dict.get valueId values)
                    |> Maybe.andThen (\value -> getOneStringFromValueType language values value.value)
            )
        |> oneOfMaybes


getOneStringFromValueType : Language -> Dict String TypedValue -> ValueType -> Maybe String
getOneStringFromValueType language values valueType =
    case valueType of
        BijectiveCardReferenceValue _ ->
            Nothing

        BooleanValue _ ->
            Nothing

        CardIdArrayValue _ ->
            Nothing

        CardIdValue cardId ->
            Nothing

        EmailValue value ->
            Just value

        ImagePathValue path ->
            Just path

        LocalizedStringValue valueByLanguage ->
            getValueByPreferredLanguage language valueByLanguage

        NumberValue _ ->
            Nothing

        StringValue value ->
            Just value

        UrlValue value ->
            Just value

        ValueIdArrayValue [] ->
            Nothing

        ValueIdArrayValue (childValue :: _) ->
            getOneStringFromValueType language values (ValueIdValue childValue)

        ValueIdValue valueId ->
            Dict.get valueId values
                |> Maybe.andThen (\subValue -> getOneStringFromValueType language values subValue.value)

        WrongValue _ _ ->
            Nothing


getName : Language -> Card -> Dict String TypedValue -> String
getName language card values =
    -- Note: Name can be Nothing, if down-voted.
    getOneString language nameKeyIds card values
        |> Maybe.withDefault (translate language UntitledCard)


getLocalizedStringFromValueId : Language -> Dict String TypedValue -> String -> String
getLocalizedStringFromValueId language values valueId =
    case Dict.get valueId values of
        Nothing ->
            "Error: value not found for ID: " ++ valueId

        Just { value } ->
            case value of
                LocalizedStringValue localizedValues ->
                    getValueByPreferredLanguage language localizedValues
                        |> Maybe.withDefault ("No localization for string valueId=" ++ valueId)

                _ ->
                    "This should not happen"


getSubTypes : Language -> Card -> Dict String TypedValue -> List String
getSubTypes language card values =
    List.map
        (getLocalizedStringFromValueId language values)
        card.subTypeIds


getTags : Language -> Card -> Dict String TypedValue -> List { tag : String, tagId : String }
getTags language card values =
    List.map
        (\tagId ->
            { tag = getLocalizedStringFromValueId language values tagId
            , tagId = tagId
            }
        )
        card.tagIds


getUsages : Language -> Card -> Dict String TypedValue -> List { tag : String, tagId : String }
getUsages language card values =
    List.map
        (\tagId ->
            { tag = getLocalizedStringFromValueId language values tagId
            , tagId = tagId
            }
        )
        card.usageIds


getValueByPreferredLanguage : Language -> Dict String String -> Maybe String
getValueByPreferredLanguage language valueByLanguage =
    let
        userLanguageCode =
            iso639_1FromLanguage language
    in
        ([ Dict.get userLanguageCode valueByLanguage
            |> Maybe.map (\s -> ( userLanguageCode, s ))
         , Dict.get "en" valueByLanguage
            |> Maybe.map (\s -> ( "en", s ))
         ]
            ++ (Dict.toList valueByLanguage |> List.map Just)
        )
            |> List.filterMap identity
            |> List.filterMap
                (\( languageCode, s ) ->
                    if String.isEmpty (String.trim s) then
                        Nothing
                    else
                        -- (if languageCode == userLanguageCode then
                        --     s
                        --  else
                        --     "(" ++ (String.toUpper languageCode) ++ ") " ++ s
                        -- )
                        Just s
                )
            |> List.head


languageFromIso639_1 : String -> Maybe Language
languageFromIso639_1 str =
    case str of
        "en" ->
            Just English

        "es" ->
            Just Spanish

        "fr" ->
            Just French

        _ ->
            Nothing


iso639_1FromLanguage : Language -> String
iso639_1FromLanguage language =
    case language of
        English ->
            "en"

        Spanish ->
            "es"

        French ->
            "fr"


{-| Pick the first `Maybe` that actually has a value. Useful when you want to
try a couple different things, but there is no default value.

    oneOf [ Nothing, Just 42, Just 71 ] == Just 42
    oneOf [ Nothing, Nothing, Just 71 ] == Just 71
    oneOf [ Nothing, Nothing, Nothing ] == Nothing

-}
oneOfMaybes : List (Maybe a) -> Maybe a
oneOfMaybes maybes =
    case maybes of
        [] ->
            Nothing

        maybe :: rest ->
            case maybe of
                Nothing ->
                    oneOfMaybes rest

                Just _ ->
                    maybe


translate : Language -> TranslationId -> String
translate language translationId =
    let
        translationSet =
            getTranslationSet translationId

        translateHelp language =
            case language of
                English ->
                    translationSet.english

                French ->
                    translationSet.french

                Spanish ->
                    translationSet.spanish
    in
        oneOfMaybes
            [ translateHelp language
            , translateHelp English

            -- |> Maybe.map (\str -> "(EN) " ++ str)
            ]
            |> Maybe.withDefault
                ("TODO translate the ID "
                    ++ (toString translationId)
                    ++ " in "
                    ++ (toString language)
                )
