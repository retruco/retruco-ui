module I18n exposing (..)

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
    | AccountCreationFailed
    | ActivationDescription
    | ActivationFailed
    | ActivationInProgress
    | ActivationNotRequested
    | ActivationSucceeded
    | ActivationTitle
    | Actor GrammaticalNumber
    | AddNew
    | AddNewItemBox
    | AdditionalInformations
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
    | CardId
    | CardIdArray
    | ChangePassword
    | ChangePasswordDescription
    | ChangePasswordExplanation
    | ChangePasswordTitle
    | Close
    | Colon
    | Copyright
    | CountVersionsAvailable Int
    | Create
    | CreateAccountNow
    | CreateYourAccount
    | Email
    | EmailPlaceholder
    | EmailSentForAccountActivation String
    | EmptyString
    | EnterBoolean
    | EnterEmail
    | EnterImage
    | EnterNumber
    | EnterPassword
    | EnterUrl
    | EnterUsername
    | EnterValue
    | EveryLanguage
    | FalseWord
    | FooterAbout
    | FooterDiscover
    | GenericError
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
    | LocalizedString
    | MissingDescription
    | MissingValue
    | NetworkError
    | NetworkErrorExplanation
    | New
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
    | ReadingSelectedImage
    | ReadMore
    | Register
    | RegisterNow
    | ResetPassword
    | ResetPasswordDescription
    | ResetPasswordExplanation
    | ResetPasswordLink
    | ResetPasswordTitle
    | Save
    | Score
    | Search
    | SearchPlaceholder
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
    | Tags
    | TextField
    | Timeout
    | TimeoutExplanation
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
    | ValuePlaceholder
    | Values
    | ValuesDescription
    | VoteBestContributions
    | Website


getTranslationSet : TranslationId -> TranslationSet
getTranslationSet translationId =
    case translationId of
        About ->
            { english = s "About"
            , french = s "À propos"
            , spanish = todo
            }

        AboutCredits ->
            { english = s "Credits"
            , french = s "Crédits"
            , spanish = s "Créditos"
            }

        AboutCreditsContent ->
            { english = s "The bubble tags navigation system is based on "
            , french = s "Le système de navigations des tag par bulles est basé sur la solution "
            , spanish = todo
            }

        AboutDescription ->
            getTranslationSet AboutLead

        AboutLead ->
            { english = s "About the OGP Toolbox"
            , french = s "À propos de la boite à outils OGP"
            , spanish = todo
            }

        AboutLegal ->
            { english = s "Legal notices"
            , french = s "Mentions légales"
            , spanish = s "Nota legal"
            }

        AboutLegalContent ->
            { english = s "OGPtoolobox.org is edited by the Etalab taskforce, a Prime Minister service, 39 quai André Citroën 75015 PARIS."
            , french = s "OGPtoolobox.org est édité par la mission Etalab, service du Premier Ministre, 39 quai André Citroën 75015 PARIS."
            , spanish = todo
            }

        AccountCreationFailed ->
            { english = s "Account création failed"
            , french = s "Échec de la création du compte"
            , spanish = todo
            }

        ActivationDescription ->
            { english = s "Verification of the user's email address"
            , french = s "Vérification de l'adresse courriel de l'utilisateur"
            , spanish = todo
            }

        ActivationFailed ->
            { english = s "Email address verfication failed"
            , french = s "Échec de la vérification de l'adresse courriel"
            , spanish = todo
            }

        ActivationInProgress ->
            { english = s "Verifying your email address..."
            , french = s "Vérification de votre adresse courriel..."
            , spanish = todo
            }

        ActivationNotRequested ->
            { english = s "Your email address will be verified shortly..."
            , french = s "Votre adresse courriel va bientôt être vérifiée..."
            , spanish = todo
            }

        ActivationSucceeded ->
            { english = s "The verification of your email address has succeeded. Your account is now activated!"
            , french = s "La vérification de votre adresse courriel a réussi. Votre compte est maintenant activé !"
            , spanish = todo
            }

        ActivationTitle ->
            { english = s "Account Activation"
            , french = s "Activation du compte"
            , spanish = todo
            }

        Actor number ->
            { english =
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

        AdditionalInformations ->
            { english = s "Additional informations"
            , french = s "Informations supplémentaires"
            , spanish = todo
            }

        AuthenticationFailed ->
            { english = s "Authentication failed"
            , french = s "L'authentification a échoué"
            , spanish = todo
            }

        AuthenticationRequired ->
            { english = s "Authentication required"
            , french = todo
            , spanish = todo
            }

        AuthenticationRequiredExplanation ->
            { english = s "You must sign in to display this page."
            , french = todo
            , spanish = todo
            }

        AddNew ->
            { english = s "Add new"
            , french = s "Ajouter"
            , spanish = todo
            }

        AddNewItemBox ->
            { english = s "Add a new item"
            , french = s "Ajouter un nouvel élément"
            , spanish = todo
            }

        BadAuthorization ->
            { english = s "Authorization code is wrong or obsolete."
            , french = s "Le code d'autorisation est erroné ou périmé."
            , spanish = todo
            }

        BadEmailOrPassword ->
            { english = s "Either email address is unknown or password is wrong."
            , french = s "Soit l'adresse courriel est inconnue, soit le mot de passe est erroné."
            , spanish = todo
            }

        BadPayload ->
            { english = s "Bad payload"
            , french = s "Contenu incorrect"
            , spanish = todo
            }

        BadPayloadExplanation ->
            { english = s "The server returned unexpected data."
            , french = s "Le server a retourné des données imprévues"
            , spanish = todo
            }

        BadStatus ->
            { english = s "Bad status"
            , french = s "Statut incorrect"
            , spanish = todo
            }

        BadUrl ->
            { english = s "Bad URL"
            , french = s "URL incorrecte"
            , spanish = todo
            }

        BadUrlExplanation ->
            { english = s "The given URL is invalid."
            , french = s "L'URL fournie n'est pas valide."
            , spanish = todo
            }

        BestOf count ->
            { english = s ("Best of " ++ (toString count))
            , french = s ("Meilleur parmi " ++ (toString count))
            , spanish = todo
            }

        BijectiveCardReference ->
            { english = s "Bijective reference to a card"
            , french = s "Référence bijective à une fiche"
            , spanish = todo
            }

        Boolean ->
            { english = s "Boolean"
            , french = s "Booléen"
            , spanish = todo
            }

        BooleanField ->
            getTranslationSet Boolean

        Cancel ->
            { english = s "Cancel"
            , french = s "Annuler"
            , spanish = todo
            }

        CardId ->
            { english = s "Reference to a card"
            , french = s "Référence à une fiche"
            , spanish = todo
            }

        CardIdArray ->
            { english = s "Array of references to cards"
            , french = s "Tableau de références à des fiches"
            , spanish = todo
            }

        ChangePassword ->
            { english = s "Change your password"
            , french = s "Changez votre mot de passe"
            , spanish = todo
            }

        ChangePasswordDescription ->
            { english = s "Change of the user's password"
            , french = s "Changement du mot de passe de l'utilisateur"
            , spanish = todo
            }

        ChangePasswordExplanation ->
            { english = s "Enter a new password to be able to sign-in."
            , french = s "Entrez un nouveau mot de passe qui vous servira à vous identifier."
            , spanish = todo
            }

        ChangePasswordTitle ->
            { english = s "Password Change"
            , french = s "Changement du mot de passe"
            , spanish = todo
            }

        Close ->
            { english = s "Close"
            , french = s "Fermer"
            , spanish = todo
            }

        Colon ->
            { english = s ": "
            , french = s " : "
            , spanish = s ": "
            }

        Copyright ->
            { english = s "© 2016 Etalab. Design by Nodesign.net"
            , french = s "© 2016 Etalab. Design par Nodesign.net"
            , spanish = todo
            }

        CountVersionsAvailable count ->
            { english =
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

        Create ->
            { english = s "Create"
            , french = s "Créer"
            , spanish = todo
            }

        CreateAccountNow ->
            { english = s "Create your account now"
            , french = s "Créez votre compte maintenant"
            , spanish = todo
            }

        CreateYourAccount ->
            { english = s "Create your account"
            , french = s "Créez votre compte"
            , spanish = todo
            }

        Email ->
            { english = s "Email"
            , french = s "Courriel"
            , spanish = todo
            }

        EmailPlaceholder ->
            { english = s "john.doe@example.com"
            , french = s "martine.dupont@exemple.fr"
            , spanish = todo
            }

        EmailSentForAccountActivation email ->
            { english =
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
            { english = s ""
            , french = s ""
            , spanish = s ""
            }

        EnterBoolean ->
            { english = s "Please check or uncheck the box"
            , french = s "Veuillez cocher ou décocher la case"
            , spanish = todo
            }

        EnterEmail ->
            { english = s "Please enter your email"
            , french = s "Veuillez entrer votre courriel"
            , spanish = todo
            }

        EnterImage ->
            { english = s "Please select an image"
            , french = s "Veuillez sélectionner une image"
            , spanish = todo
            }

        EnterNumber ->
            { english = s "Please enter a number"
            , french = s "Veuillez entrer un nombre"
            , spanish = todo
            }

        EnterPassword ->
            { english = s "Please enter your password"
            , french = s "Veuillez entrer votre mot de passe"
            , spanish = todo
            }

        EnterUrl ->
            { english = s "Please enter a link (an URL)"
            , french = s "Veuillez entrer un lien (une URL)"
            , spanish = todo
            }

        EnterUsername ->
            { english = s "Please enter your username"
            , french = s "Veuillez entrer votre nom d'utilisateur"
            , spanish = todo
            }

        EnterValue ->
            { english = s "Please enter value"
            , french = s "Veuillez entrer une valeur"
            , spanish = todo
            }

        EveryLanguage ->
            { english = s "Every language"
            , french = s "Toutes les langues"
            , spanish = todo
            }

        FalseWord ->
            { english = s "False"
            , french = s "Faux"
            , spanish = todo
            }

        FooterAbout ->
            { english = s "About"
            , french = s "A propos"
            , spanish = s "Acerca"
            }

        FooterDiscover ->
            { english = s "Discover"
            , french = s "Découvrir"
            , spanish = s "Descubrir"
            }

        GenericError ->
            { english = s "Something wrong happened!"
            , french = s "Quelque chose s'est mal passé !"
            , spanish = todo
            }

        HeaderTitle ->
            { english = s "digital solutions to improve democracy"
            , french = s "solutions numériques pour la démocratie"
            , spanish = todo
            }

        Help ->
            { english = s "Help"
            , french = s "Aide"
            , spanish = s "Ayuda"
            }

        Home ->
            { english = s "Home"
            , french = s "Accueil"
            , spanish = s "Inicio"
            }

        HomeDescription ->
            { english = s "Digital solutions to improve democracy"
            , french = s "Solutions numériques pour la démocratie"
            , spanish = todo
            }

        HomeTitle ->
            { english = s "OGP Toolbox"
            , french = s "OGP Toolbox"
            , spanish = todo
            }

        Image ->
            { english = s "Image"
            , french = s "Image"
            , spanish = todo
            }

        ImageAlt ->
            { english = s "The uploaded image"
            , french = s "L'image téléversée"
            , spanish = todo
            }

        ImageField ->
            getTranslationSet Image

        ImageUploadError message ->
            { english = s ("Image upload error: " ++ message)
            , french = s ("Échec du téléversement de l'image :" ++ message)
            , spanish = todo
            }

        ImproveExistingContent ->
            { english = s "Improve existing content"
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
            { english = s "Not a valid number"
            , french = s "Ce n'est pas un nombre valide."
            , spanish = todo
            }

        Language language ->
            case language of
                English ->
                    { english = s "English"
                    , french = s "Anglais"
                    , spanish = s "Inglés"
                    }

                French ->
                    { english = s "French"
                    , french = s "Français"
                    , spanish = s "Francés"
                    }

                Spanish ->
                    { english = s "Spanish"
                    , french = s "Espagnol"
                    , spanish = s "Español"
                    }

        LanguageWord ->
            { english = s "Language"
            , french = s "Langue"
            , spanish = s "Idioma"
            }

        License ->
            { english = s "License"
            , french = s "Licence"
            , spanish = todo
            }

        LocalizedString ->
            { english = s "Localized string"
            , french = s "Chaîne de caractères localisée"
            , spanish = todo
            }

        MissingDescription ->
            { english = s "Missing description"
            , french = s "Description manquante"
            , spanish = todo
            }

        MissingValue ->
            { english = s "Missing value"
            , french = s "Valeur manquante"
            , spanish = todo
            }

        NetworkError ->
            { english = s "Network error"
            , french = s "Erreur réseau"
            , spanish = todo
            }

        NetworkErrorExplanation ->
            { english = s "There was a network error."
            , french = todo
            , spanish = todo
            }

        New ->
            { english = s "New"
            , french = s "Nouveau"
            , spanish = todo
            }

        NewValue ->
            { english = s "New Value"
            , french = s "Nouvelle valeur"
            , spanish = todo
            }

        NewValueDescription ->
            { english = s "Form to enter a new value"
            , french = s "Formulaire de création d'une nouvelle valeur"
            , spanish = todo
            }

        Number ->
            { english = s "Number"
            , french = s "Nombre"
            , spanish = todo
            }

        NumberPlaceholder ->
            { english = s "3.1415927"
            , french = s "3.1415927"
            , spanish = s "3.1415927"
            }

        Objects ->
            { english = s "Objects"
            , french = s "Objets"
            , spanish = todo
            }

        ObjectsDescription ->
            { english = s "List of objects"
            , french = s "Liste des objets"
            , spanish = todo
            }

        PageLoading ->
            { english = s "Page is loading"
            , french = s "Chargement en cours"
            , spanish = todo
            }

        PageLoadingExplanation ->
            { english = s "Data is loading and should be displayed quite soon."
            , french = todo
            , spanish = todo
            }

        PageNotFound ->
            { english = s "Page Not Found"
            , french = s "Page non trouvée"
            , spanish = todo
            }

        PageNotFoundDescription ->
            { english = s "The request pas doesn't exist."
            , french = s "La page demandée n'existe pas."
            , spanish = todo
            }

        PageNotFoundExplanation ->
            { english = s "Sorry, but the page you were trying to view does not exist."
            , french = s "Désolé mais la page que vous avez demandé n'est pas disponible"
            , spanish = todo
            }

        Password ->
            { english = s "Password"
            , french = s "Mot de passe"
            , spanish = todo
            }

        PasswordChangeFailed ->
            { english = s "Password change failed"
            , french = s "Échec du changement de mot de passe"
            , spanish = todo
            }

        PasswordLost ->
            { english = s "Password lost?"
            , french = s "Mot de passe oublié ?"
            , spanish = todo
            }

        PasswordPlaceholder ->
            { english = s "Your secret password"
            , french = s "Votre mot de passe secret"
            , spanish = todo
            }

        ReadingSelectedImage ->
            { english = s "Reading selected image..."
            , french = s "Lecture de l'image sélectionnée..."
            , spanish = todo
            }

        ReadMore ->
            { english = s "Read more"
            , french = s "En savoir plus"
            , spanish = todo
            }

        Register ->
            { english = s "Register"
            , french = s "Créer le compte"
            , spanish = todo
            }

        RegisterNow ->
            { english = s "Register now!"
            , french = s "Inscrivez vous maintenant !"
            , spanish = todo
            }

        ResetPassword ->
            { english = s "Reset Password"
            , french = s "Changer de mot de passe"
            , spanish = todo
            }

        ResetPasswordDescription ->
            { english = s "Reset of the user's password"
            , french = s "Réinitialisation du mot de passe de l'utilisateur"
            , spanish = todo
            }

        ResetPasswordExplanation ->
            { english = s "Enter your email. We will send you the instructions to create a new password."
            , french = s "Entrez votre courriel. Nous vous enverrons les instructions pour changer de mot de passe."
            , spanish = todo
            }

        ResetPasswordLink ->
            { english = s "I forgot my password"
            , french = s "J'ai oublié mon mot de passe"
            , spanish = todo
            }

        ResetPasswordTitle ->
            { english = s "Password Reset"
            , french = s "Réinitialisation du mot de passe"
            , spanish = todo
            }

        Save ->
            { english = s "Save"
            , french = s "Enregistrer"
            , spanish = todo
            }

        Score ->
            { english = s "Score"
            , french = s "Score"
            , spanish = s "Score"
            }

        Search ->
            { english = s "Search"
            , french = s "Rechercher"
            , spanish = todo
            }

        SearchPlaceholder ->
            { english = s "Search terms"
            , french = s "Termes de la recherche"
            , spanish = todo
            }

        Send ->
            { english = s "Send"
            , french = s "Envoyer"
            , spanish = todo
            }

        SendEmailAgain ->
            { english = s "Send email again"
            , french = s "Réenvoyer le courriel"
            , spanish = todo
            }

        Share ->
            { english = s "Share"
            , french = s "Partager"
            , spanish = todo
            }

        ShowAll count ->
            { english = s ("Show all " ++ (toString count))
            , french = s ("Voir tous (" ++ (toString count) ++ ")")
            , spanish = s ("Ver todo (" ++ (toString count) ++ ")")
            }

        SignIn ->
            { english = s "Sign In"
            , french = s "Connexion"
            , spanish = s "Acceder"
            }

        SignInDescription ->
            { english = s "User's sign in"
            , french = s "Identification de l'utilisateur"
            , spanish = todo
            }

        SignInTitle ->
            { english = s "Sign In"
            , french = s "Connexion"
            , spanish = todo
            }

        SignInToContribute ->
            { english = s "Sign in to contribute"
            , french = s "Identifiez-vous pour contribuer"
            , spanish = todo
            }

        SignOut ->
            { english = s "Sign Out"
            , french = s "Déconnexion"
            , spanish = s "Salir"
            }

        SignOutAndContributeLater ->
            { english = s "Sign out and contribute later"
            , french = s "Déconnectez-vous et contribuez plus tard"
            , spanish = todo
            }

        SignOutDescription ->
            { english = s "User's sign out"
            , french = s "Déconnexion de l'utilisateur"
            , spanish = todo
            }

        SignOutTitle ->
            { english = s "Sign Out"
            , french = s "Déconnexion"
            , spanish = todo
            }

        SignUp ->
            { english = s "Sign Up"
            , french = s "Inscription"
            , spanish = s "Registrarse"
            }

        SignUpDescription ->
            { english = s "User's sign up"
            , french = s "Inscription de l'utilisateur"
            , spanish = todo
            }

        SignUpTitle ->
            { english = s "Sign Up"
            , french = s "Inscription"
            , spanish = todo
            }

        String ->
            { english = s "String"
            , french = s "Chaîne de caractères"
            , spanish = todo
            }

        Tags ->
            { english = s "Tags"
            , french = s "Tags"
            , spanish = s "Tags"
            }

        TextField ->
            { english = s "Text"
            , french = s "Texte"
            , spanish = todo
            }

        Timeout ->
            { english = s "Timeout"
            , french = s "Délai dépassé"
            , spanish = todo
            }

        TimeoutExplanation ->
            { english = s "The server was too slow to respond."
            , french = s "Le serveur a mis trop de temps à repondre."
            , spanish = todo
            }

        TrueWord ->
            { english = s "True"
            , french = s "Vrai"
            , spanish = todo
            }

        TweetMessage name url ->
            { english = s ("Discover " ++ name ++ " on OGPToolbox.org: " ++ url)
            , french = s ("Découvrez " ++ name ++ " dans OGPToolbox.org : " ++ url)
            , spanish = todo
            }

        Type ->
            { english = s "Type"
            , french = s "Type"
            , spanish = s "Tipo"
            }

        UnknownLanguage ->
            { english = s "Unknown language"
            , french = s "Langue inconnue"
            , spanish = todo
            }

        UnknownSchemaId schemaId ->
            { english = s ("Reference to an unknown schema: " ++ schemaId)
            , french = s ("Référence à un schema inconnu : " ++ schemaId)
            , spanish = todo
            }

        UnknownUser ->
            { english = s "User is unknown."
            , french = s "L'utilisateur est inconnu."
            , spanish = todo
            }

        UnknownValue ->
            { english = s "Unknown value"
            , french = s "Valeur inconnue"
            , spanish = todo
            }

        UntitledCard ->
            { english = s "Untitled Card"
            , french = s "Fiche sans titre"
            , spanish = s "Tipo"
            }

        UploadImage ->
            { english = s "Upload image"
            , french = s "Téléverser une image"
            , spanish = todo
            }

        UploadingImage filename ->
            { english = s ("Uploading image \"" ++ filename ++ "\"...")
            , french = s ("Téléversement de l'image \"" ++ filename ++ "\"...")
            , spanish = todo
            }

        Url ->
            { english = s "Link (URL)"
            , french = s "Lien (URL)"
            , spanish = todo
            }

        UrlPlaceholder ->
            { english = s "https://www.example.com/sample-page"
            , french = s "https://www.exemple.fr/exemple-de-page"
            , spanish = todo
            }

        Username ->
            { english = s "Username"
            , french = s "Nom d'utilisateur"
            , spanish = todo
            }

        UsernameOrEmailAlreadyExist ->
            { english = s "Username or email are already used."
            , french = s "Le nom d'utilisateur ou le mot de passe sont déjà utilisés."
            , spanish = todo
            }

        UsernamePlaceholder ->
            { english = s "John Doe"
            , french = s "Françoise Martin"
            , spanish = todo
            }

        UserProfileDescription ->
            { english = s "The profile of user"
            , french = s "Le profil de l'utilisation"
            , spanish = todo
            }

        Uses ->
            { english = s "Uses"
            , french = s "Utilise"
            , spanish = todo
            }

        Value ->
            { english = s "Value"
            , french = s "Valeur"
            , spanish = todo
            }

        ValueCreationFailed ->
            { english = s "Value création failed"
            , french = s "Échec de la création de la valeur"
            , spanish = todo
            }

        ValueId ->
            { english = s "Reference to a value"
            , french = s "Référence à une valeur"
            , spanish = todo
            }

        ValueIdArray ->
            { english = s "Array of references to valuess"
            , french = s "Tableau de références à des valeurs"
            , spanish = todo
            }

        ValuePlaceholder ->
            { english = s "The value..."
            , french = s "La valeur"
            , spanish = todo
            }

        Values ->
            { english = s "Values"
            , french = s "Valeurs"
            , spanish = todo
            }

        ValuesDescription ->
            { english = s "List of values"
            , french = s "Liste de valeurs"
            , spanish = todo
            }

        VoteBestContributions ->
            { english = s "Vote for the best contributions"
            , french = s "Votez pour les meilleurs contributions"
            , spanish = todo
            }

        Website ->
            { english = s "Website"
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

        ImagePathValue path ->
            Nothing

        LocalizedStringValue valueByLanguage ->
            getValueByPreferredLanguage language valueByLanguage

        NumberValue _ ->
            Nothing

        StringValue value ->
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
    getOneString language nameKeys card values
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
