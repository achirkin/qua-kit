{-# OPTIONS_HADDOCK hide, prune #-}
{-# Language CPP #-}
module Foundation
    ( module Foundation
    , parseSqlKey
    ) where

#if DEVELOPMENT
import qualified Prelude
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
#endif
import Control.Concurrent.STM.TChan
import qualified Data.Text as Text
import qualified Data.Map as Map
import Yesod.Auth.LdapNative
import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool, toSqlKey, fromSqlKey, Single (..), rawSql)
import qualified Network.Mail.Mime as Mail
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Text.Shakespeare.Text (stext)
import Yesod.Auth.Email
import Yesod.Auth.Message   (AuthMessage (..))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Data.Text.Read (decimal)
import System.Directory (createDirectoryIfMissing)
import Text.Blaze (Markup)

import Handler.Mooc.EdxLogin
import Model.Session
import Application.Edx


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appWSChan      :: TChan Text
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
#if DEVELOPMENT
mkYesodData "App" $(do
  routes <- TH.runIO $ do
    a <- Prelude.readFile "config/routes"
    b <- Prelude.readFile "config/routes.dev"
    return $ Prelude.unlines [a,b]
  TH.quoteExp parseRoutes routes
  )
#else
mkYesodData "App" $(parseRoutesFile "config/routes")
#endif

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req -> fromMaybe (getApprootText guessApproot app req)
                                                     (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
      liftIO $ createDirectoryIfMissing True "config"
      Just <$> defaultClientSessionBackend
        43200    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
--        master <- getYesod
        mmsg <- getMessage
        muser <- fmap entityVal <$> maybeAuth
        guessedExerciseId <- show . fromSqlKey <$> getCurrentExercise
        siteMenu <- pageBody <$> widgetToPageContent $(widgetFile "site-menu")

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent widget
--        pc <- widgetToPageContent $ do
--            addStylesheet $ StaticR css_bootstrap_css
            --  (widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/site-layout.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized AdminR _ = do
      role <- muserRole <$> maybeAuth
      return $ if role == UR_ADMIN
               then Authorized
               else Unauthorized "You need admin rights to access this page."
    isAuthorized CompareProposalsR _ = do
      iAmHere <- (Nothing /=) <$> maybeAuthId
      return $ if iAmHere
               then Authorized
               else Unauthorized "Only logged-in users can access this page"
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    maximumContentLength _ _ = Just 4000000


fullLayout :: Maybe Markup -> Text -> Widget -> Handler Html
fullLayout mmsgIcon defaultMsg widget = do
    mmsg <- getMessage
    muser <- fmap entityVal <$> maybeAuth
    guessedExerciseId <- show . fromSqlKey <$> getCurrentExercise
    siteMenu <- pageBody <$> widgetToPageContent $(widgetFile "site-menu")
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/site-layout-full.hamlet")


adminLayout :: Text -> Widget -> Handler Html
adminLayout defaultMsg widget = do
    mmsg <- getMessage
    muser <- fmap entityVal <$> maybeAuth
    guessedExerciseId <- show . fromSqlKey <$> getCurrentExercise
    siteMenu <- pageBody <$> widgetToPageContent $(widgetFile "site-menu")
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/site-layout-admin.hamlet")


minimalLayout :: Widget -> Handler Html
minimalLayout widget = do
    muser <- fmap entityVal <$> maybeAuth
    guessedExerciseId <- show . fromSqlKey <$> getCurrentExercise
    siteMenu <- pageBody <$> widgetToPageContent $(widgetFile "site-menu")
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/site-layout-minimal.hamlet")


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = MoocHomeR
    -- Where to send a user after logout
    logoutDest _ = MoocHomeR
    onLogout = clearSession
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    -- authenticatin using ldap plugin
    authenticate creds@Creds{credsPlugin = "ldap"} = runDB $ do
        x <- getBy . ETHUserName . Just $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userName = credsIdent creds
                , userRole = UR_LOCAL
                , userEthUserName = Just $ credsIdent creds
                , userEdxUserId = Nothing
                , userEmail = Nothing
                , userPassword = Nothing
                , userVerified = True
                }
    authenticate creds@Creds{credsPlugin = "lti"} = do
      uid <- runDB $ do
        x <- getBy . EdxUserId . Just $ credsIdent creds
        case x of
          Just (Entity uid _) -> return uid
          Nothing -> insert User
              { userName = "anonymous edX student"
              , userRole = UR_STUDENT
              , userEthUserName = Nothing
              , userEdxUserId = Just $ credsIdent creds
              , userEmail = Nothing
              , userPassword = Nothing
              , userVerified = True
              }
      setupEdxGrading uid (credsExtra creds)
      case Map.lookup "custom_exercise_type" (Map.fromList $ credsExtra creds) of
          Just "design"  -> setUltDest RedirectToQuaViewEditR
          Just "compare" -> setUltDest CompareProposalsR
          _ -> return ()
      return $ Authenticated uid
    authenticate Creds{credsPlugin = "temporary"} = do
      uid <- runDB $
        insert User
              { userName = "Anonymous user"
              , userRole = UR_STUDENT
              , userEthUserName = Nothing
              , userEdxUserId = Nothing
              , userEmail = Nothing
              , userPassword = Nothing
              , userVerified = False
              }
      return $ Authenticated uid
    -- use Yesod.Auth.Email plugin,
    -- also for legacy usernames that are stored in the email column:
    -- Look at https://hackage.haskell.org/package/yesod-auth-1.4.19/docs/src/Yesod-Auth-Email.html#postLoginR
    -- according to that page, 'credsPlugin' record of creds can be one of
    --  "email", "email-verified", "username", or who knows what else.
    -- That is why we need this weird check below.
    authenticate creds@(Creds cPlugin _ _)
      | "email" `isPrefixOf` cPlugin || "username" == cPlugin
        = runDB $ do
          x <- getBy . UserEmailId . Just $ credsIdent creds
          return $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing             -> UserError AuthError

    authenticate _ = do
      return $ UserError AuthError

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins ye = [ authLdapWithForm ldapConf $ \ldapLoginR -> $(widgetFile "login-ethz")
                     , authLtiPlugin (appLTICredentials $ appSettings ye)
                     , authEmail]

    authHttpManager = getHttpManager

ldapConf :: LdapAuthConf
ldapConf = setHost (Secure "isla.ethz.ch") $ setPort 636
  $ mkLdapConf Nothing "cn=users,dc=isla,dc=ethz,dc=ch"



instance YesodAuthPersist App

instance YesodAuthEmail App where
  type AuthEmailId App = UserId

  afterPasswordRoute _ = RedirectToQuaViewEditR
  confirmationEmailSentResponse email = do
    setMessage $ "A confirmain email has been sent. Please, check your mailbox."
    setCreds False $ Creds "email" email []
    mscId <- maybeEnroll
    redirect $ case mscId of
      Nothing -> MoocHomeR
      Just _  -> RedirectToQuaViewEditR


  addUnverified email verkey = do
    uid <- runDB $ do
      uid <- insert User
              { userName = takeWhile (/= '@') email
              , userRole = UR_NOBODY
              , userEthUserName = Nothing
              , userEdxUserId = Nothing
              , userEmail = Just email
              , userPassword = Nothing
              , userVerified = False
              }
      _ <- insert $ UserProp uid "verkey" verkey
      return uid
    maybeSetRoleBasedOnParams uid

    pure uid


  -- this function must not log-in user,
  --   because it is actually used by admin user too!
  --   (it would lead to admin being re-logged-in as as the new user)
  sendVerifyEmail email _ verurl = do
    -- Print out to the console the verification email, for easier
    -- debugging.
    $(logDebug) $ "Copy/ Paste this URL in your browser: " <> verurl

    -- Send email.
    mailFrom <- appMailFrom
    _ <- fork $ catch
      (liftIO . Mail.renderSendMail $ Mail.simpleMail'
        (Mail.Address Nothing email) -- To address
        mailFrom
        "Please validate your email address" -- Subject
        [stext|
            Please confirm your email address by clicking on the link below.

            #{verurl}

            Thank you
        |]
      )
      (\e -> $(logWarn) $ "[EMAIL REGISTRATION] Could not send an email: " <> Text.pack (show (e :: SomeException)))
    pure ()
    -- setCreds False $ Creds "email" email []
    -- mscId <- maybeEnroll
    -- setUltDest $ case mscId of
    --   Nothing -> MoocHomeR
    --   Just _  -> RedirectToQuaViewEditR

  getVerifyKey uid = runDB $ do
    mup <- getBy $ UserProperty uid "verkey"
    return $ case mup of
      (Just (Entity _ u)) -> Just $ userPropValue u
      _                   -> Nothing
  setVerifyKey uid key = runDB $ do
    _ <- upsertBy (UserProperty uid "verkey") (UserProp uid "verkey" key)
                  [UserPropValue =. key]
    return ()
  verifyAccount uid = runDB $ do
    mu <- get uid
    case mu of
      Nothing -> return Nothing
      Just _ -> do
        update uid [UserVerified =. True]
        return $ Just uid
  getPassword = runDB . fmap (join . fmap userPassword) . get
  setPassword uid pass = runDB $ do
    update uid [UserPassword =. Just pass]
    -- set verkey to empty string to prevent reuse
    void $ upsertBy (UserProperty uid "verkey") (UserProp uid "verkey" "")
           [UserPropValue =. ""]
  getEmailCreds email = runDB $ do
    mu <- getBy $ UserEmailId (Just email)
    case mu of
      Nothing             -> return Nothing
      Just (Entity uid u) -> do
        mup <- getBy $ UserProperty uid "verkey"
        return $ Just $ EmailCreds
          { emailCredsId     = uid
          , emailCredsAuthId = Just uid
          , emailCredsStatus = isJust $ userPassword u
          , emailCredsVerkey = fmap (userPropValue . entityVal) mup
          , emailCredsEmail  = email
          }
  getEmail = runDB . fmap (join . fmap userEmail) . get
  emailLoginHandler toParent = $(widgetFile "login-local")
  registerHandler = do
    let extraParam key = do
            mr <- lookupGetParam key
            pure $ (,) key <$> mr
    extraParams <- mapM extraParam ["exercise", "invitation-secret"]

    toParRt <- getRouteToParent
    let allowTempUserOption = all isJust extraParams
    (widget, _) <- lift $ generateFormPost $ registrationForm (catMaybes extraParams) toParRt allowTempUserOption
    lift $ authLayout widget
    where
      registrationForm extraFields toParentRoute allowTempUserOption extra  = do
        let emailSettings = FieldSettings {
              fsLabel = SomeMessage ("E-Mail"::Text),
              fsTooltip = Nothing,
              fsId = Just "email",
              fsName = Just "email",
              fsAttrs = [("autofocus", ""), ("class", "form-control")]
            }

        (emailRes, emailView) <- mreq emailField emailSettings Nothing
        let widget  = $(widgetFile "register-local")
        return (emailRes, widget)
  setPasswordHandler needOldPw = do
    messageRender <- lift getMessageRender
    toParent <- getRouteToParent
    selectRep $ do
      provideJsonMessage $ messageRender ("Set password"::Text)
      provideRep $ lift $ authLayout $ do
        (widget, _) <- liftWidgetT $ generateFormPost (setPasswordForm toParent needOldPw)
        widget
    where
      setPasswordForm toParent needOld extra = do
        (currentPasswordRes, currentPasswordView) <- mreq passwordField currentPasswordSettings Nothing
        (newPasswordRes, newPasswordView)         <- mreq passwordField newPasswordSettings     Nothing
        (confirmPasswordRes, confirmPasswordView) <- mreq passwordField confirmPasswordSettings Nothing

        let passwordFormRes = PasswordForm <$> currentPasswordRes <*> newPasswordRes <*> confirmPasswordRes
        let widget = $(widgetFile "set-password-local")
        return (passwordFormRes, widget)
      currentPasswordSettings =
        FieldSettings {
          fsLabel = SomeMessage ("Current password"::Text),
          fsTooltip = Nothing,
          fsId = Just "currentPassword",
          fsName = Just "current",
          fsAttrs = [("autofocus", ""), ("class", "form-control")]
        }
      newPasswordSettings =
        FieldSettings {
          fsLabel = SomeMessage ("New password"::Text),
          fsTooltip = Nothing,
          fsId = Just "newPassword",
          fsName = Just "new",
          fsAttrs = [ ("autofocus", "")
                    , (":not", ""), ("needOld:autofocus", "")
                    , ("class", "form-control")
                    ]
        }
      confirmPasswordSettings =
        FieldSettings {
          fsLabel = SomeMessage ("Confirm password"::Text),
          fsTooltip = Nothing,
          fsId = Just "confirmPassword",
          fsName = Just "confirm",
          fsAttrs = [("autofocus", ""), ("class", "form-control")]
        }
data PasswordForm = PasswordForm { _passwordCurrent :: Text, _passwordNew :: Text, _passwordConfirm :: Text }

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


muserRole :: Maybe (Entity User) -> UserRole
muserRole Nothing = UR_NOBODY
muserRole (Just (Entity _ u)) = userRole u


-- | Get two capital characters out of a name
twoCharsName :: Text -> Text
twoCharsName s = case filter (not . null) $ Text.words s of
   [name] -> Text.toUpper $ Text.take 2 name
   n1:n2:_ -> Text.toUpper $ Text.take 1 n1 <> Text.take 1 n2
   _ -> "??"



requireSession :: Text -> Text -> Handler Text
requireSession pam errstr = lookupSession pam >>= \mv -> case mv of
    Nothing -> invalidArgsI [errstr]
    Just v  -> return v


requirePostParam :: Text -> Text -> Handler Text
requirePostParam pam errstr = lookupPostParam pam >>= \mv -> case mv of
    Nothing -> invalidArgsI [errstr]
    Just v  -> return v

getCurrentExercise :: Handler ExerciseId
getCurrentExercise = do
    mtExId <- lookupSession "custom_exercise_id"
    mgExId <- (>>= parseSqlKey) <$> lookupGetParam "exercise"
    mpExId <- (>>= parseSqlKey) <$> lookupPostParam "exercise"
    maxExId <- runDB lastExerciseId
    return $ case decimal <$> mtExId of
          Just (Right (i, _)) -> toSqlKey i
          _ -> fromMaybe maxExId $ mgExId <|> mpExId

submissionR :: CurrentScenario -> Route App
submissionR cSc = SubmissionR (currentScenarioExerciseId cSc)
                              (currentScenarioAuthorId cSc)

submissionPreviewR :: CurrentScenario -> Route App
submissionPreviewR cSc = ProposalPreviewR (currentScenarioExerciseId cSc)
                                          (currentScenarioAuthorId cSc)


lastExerciseId :: ReaderT SqlBackend Handler ExerciseId
lastExerciseId = getVal <$> rawSql query []
  where
    getVal (Single c:_)  = c
    getVal [] = toSqlKey 0
    query = "SELECT coalesce(max(id), 0) FROM exercise;"

maybeSetRoleBasedOnParams :: UserId -> Handler ()
maybeSetRoleBasedOnParams userId = do
    runDB $ update userId [UserRole =. UR_STUDENT]

maybeEnroll :: Handler (Maybe ExerciseId)
maybeEnroll = do
    mExId <- checkInvitationParams
    musrId <- maybeAuthId -- This is Nothing!
    $(logDebug) $ "New user: " <> tshow (mExId, musrId)
    case (mExId, musrId) of
        (Just exId, Just usrId) -> do
          $(logDebug) $ "Enrolling new user in exercise " <> tshow (fromSqlKey exId)
          setSafeSession userSessionCurrentExerciseId exId
          time   <- liftIO getCurrentTime
          void $ runDB $ upsert (UserExercise usrId exId time)
            [ UserExerciseUserId =. usrId
            , UserExerciseExerciseId =. exId
            , UserExerciseEnrolled =. time]
        _ -> pure ()
    return mExId

invitationParams :: ExerciseId -> Handler [(Text, Text)]
invitationParams spId = do
    sp <- runDB $ get404 spId
    pure [("exercise", tshow $ fromSqlKey spId), ("invitation-secret", exerciseInvitationSecret sp)]

checkInvitationParams :: Handler (Maybe ExerciseId)
checkInvitationParams = do
    mid <- (>>= parseSqlKey) <$> lookupPostParam "exercise"
    case mid of
       Nothing -> pure Nothing
       Just spId -> do
          sp <- runDB $ get404 spId
          is <- lookupPostParam "invitation-secret"
          pure $ if Just (exerciseInvitationSecret sp) == is
              then Just spId
              else Nothing

getTempUserR :: Handler Html
getTempUserR = do
    setCreds False $ Creds "temporary" "Anonymous user" []
    musrId <- maybeAuthId
    mExId <- (>>= parseSqlKey) <$> lookupGetParam "exercise"
    $(logDebug) $ "New user: " <> tshow (mExId, musrId)
    case (mExId, musrId) of
        (Just exId, Just usrId) -> do
          $(logDebug) $ "Enrolling new user in exercise " <> tshow (fromSqlKey exId)
          setSafeSession userSessionCurrentExerciseId exId
          time   <- liftIO getCurrentTime
          void $ runDB $ upsert (UserExercise usrId exId time)
            [ UserExerciseUserId =. usrId
            , UserExerciseExerciseId =. exId
            , UserExerciseEnrolled =. time]
        _ -> pure ()
    redirect RedirectToQuaViewEditR

postTempUserR :: Handler Html
postTempUserR = do
    setCreds False $ Creds "temporary" "Anonymous user" []
    _ <- maybeEnroll
    redirect RedirectToQuaViewEditR

-- | default mail from-address
appMailFrom :: Handler Mail.Address
appMailFrom = do
  domain <- appDomainName
  return $ Mail.Address (Just "Qua-kit") ("noreply@" <> domain)

-- | returns domain name of server
appDomainName :: Handler Text
appDomainName = do
  app <- getYesod
  req <- waiRequest
  let domainStr = getApprootText guessApproot app req
      (pref, ds) = Text.breakOn "//" domainStr
      dsClean = if Text.length ds >= 2
                then Text.drop 2 ds
                else pref
  return . fst $ Text.breakOn "/" dsClean
