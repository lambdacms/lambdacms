{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module LambdaCms.Core.Foundation where

import           Control.Arrow              ((&&&))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, toStrict)
import           Data.List                  (find, sortBy)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import           Data.Set                   (Set)
import qualified Data.Set                   as S (empty, intersection, null)
import           Data.Text                  (Text, concat, intercalate, pack,
                                             unpack)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time                  (getCurrentTime, utc)
import           Data.Time.Format.Human
import           Data.Traversable           (forM)
import           Database.Persist.Sql       (SqlBackend)
import           LambdaCms.Core.Message     (CoreMessage, defaultMessage,
                                             dutchMessage, englishMessage,
                                             russianMessage)
import qualified LambdaCms.Core.Message     as Msg
import           LambdaCms.Core.Models
import           LambdaCms.Core.Settings
import           LambdaCms.I18n
import           Network.Gravatar           (GravatarOptions(..), Size(..),
                                             def, gravatar)
import           Network.Mail.Mime
import           Network.Wai                (requestMethod)
import           Text.Hamlet                (hamletFile)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.Message         (AuthMessage (InvalidLogin))


-- | The foundation type of the subsite.
data CoreAdmin = CoreAdmin

-- | Specifies the criteria for authorizing a request.
data Allow a = AllowAll            -- ^ Allow any request (no authentication required).
             | AllowAuthenticated  -- ^ Allow requests in authenticated sessions.
             | AllowRoles a        -- ^ Allow requests in authenticated sessions belonging
                                   -- to users that have at least one matching role.
                                   -- See the `isAuthorizedTo` function for details.
             | AllowNone           -- ^ Allow no requests at all.

-- | A menu item, also see 'adminMenu'.
--
-- > MenuItem (SomeMessage MsgProduct) (ProductAdminOverviewR) "shopping-cart"
data AdminMenuItem master = MenuItem
    { label :: SomeMessage master -- ^ The text of the item (what the user sees).
    , route :: Route master       -- ^ The Route to which it points.
    , icon  :: Text               -- ^ A <http://glyphicons.bootstrapcheatsheets.com glyphicon> without the ".glyphicon-" prefix.
    }

mkYesodSubData "CoreAdmin" $(parseRoutesFile "config/routes")

instance LambdaCmsAdmin master => RenderMessage master CoreMessage where
  renderMessage = renderCoreMessage

-- | Alias for the fairly complex HandlerT type that allows persistent queries
-- on the master's db connection.
type CoreHandler a = forall master. LambdaCmsAdmin master
                   => HandlerT CoreAdmin (HandlerT master IO) a

-- | Alias for the fairly complex Form type.
type CoreForm a = forall master. LambdaCmsAdmin master
                => Html
                -> MForm (HandlerT master IO) (FormResult a, WidgetT master IO ())

class ( YesodAuth master
      , AuthId master ~ Key User
      , AuthEntity master ~ User
      , YesodAuthPersist master
      , YesodPersistBackend master ~ SqlBackend
      , ParseRoute master
      , Ord (Roles master)     -- Roles must be Ord to be a Set
      , Enum (Roles master)    -- Roles must be Enum to be able to do [minBound .. maxBound]
      , Bounded (Roles master) -- see Enum
      , Show (Roles master)    -- Roles must be Show to grant/revoke via the UI
      , Eq (Roles master)      -- Roles must be Eq for Set intersection
      ) => LambdaCmsAdmin master where

    -- | A type denoting the roles a user can have on the website.
    -- The implementation must have a datatype representing those roles. For example:
    --
    -- > type Roles MyApp = MyRoles
    --
    -- Then, in the base app, MyRoles can be:
    --
    -- @
    -- data MyRoles = Admin
    --              | SuperUser
    --              | Blogger
    --              deriving (Show, Eq, Read, Ord, Enum, Bounded)
    -- @
    type Roles master

    -- | Get all roles of a user as a Set.
    getUserRoles :: Key User -> HandlerT master IO (Set (Roles master))

    -- | Replace the current roles of a user by the given Set.
    setUserRoles :: Key User -> Set (Roles master) -> HandlerT master IO ()

    mayAssignRoles :: HandlerT master IO Bool

    -- | Gives the default roles a user should have on create
    defaultRoles :: HandlerT master IO (Set (Roles master))
    defaultRoles = return S.empty

    -- | Authorize a request to perform an action.
    -- If a user session is present it can use the specified Roles to do so.
    isAuthorizedTo :: master                      -- Needed to make function injective.
                   -> Maybe (Set (Roles master))  -- ^ Set of roles the user has.
                   -> Allow (Set (Roles master))  -- ^ Set of roles allowed to perform the action.
                   -> AuthResult
    isAuthorizedTo _ _           AllowNone          = Unauthorized "Access denied."
    isAuthorizedTo _ _           AllowAll           = Authorized
    isAuthorizedTo _ (Just _)    AllowAuthenticated = Authorized
    isAuthorizedTo _ Nothing     _                  = AuthenticationRequired
    isAuthorizedTo _ (Just urs)  (AllowRoles rrs)   = do
      case (not . S.null $ urs `S.intersection` rrs) of
        True  -> Authorized  -- non-empty intersection means authorized
        False -> Unauthorized "Access denied."

    -- | Get the 'Allow' type needed for this action.
    -- The default is that no one can do anything.
    actionAllowedFor :: Route master -- ^ The action (or route).
                     -> ByteString -- ^ The request method (e/g: GET, POST, DELETE, ...).
                                   -- Knowing /which/ method is used allows for more fine grained
                                   -- permissions than only knowing whether it is /write/ request.
                     -> Allow (Set (Roles master))
    actionAllowedFor _ _ = AllowNone

    -- | Both coreR and authR are used to navigate to a different controller.
    -- It saves you from putting "getRouteToParent" everywhere.
    coreR :: Route CoreAdmin -> Route master
    authR :: Route Auth -> Route master

    -- | Gives the route which LambdaCms should use as the master site homepage.
    masterHomeR :: Route master

    adminTitle :: SomeMessage master
    adminTitle = SomeMessage Msg.LambdaCms

    -- | Gives a widget to use as the welcome banner on the admin dashboard
    welcomeWidget :: Maybe (WidgetT master IO ())
    welcomeWidget = Just $ do
        Entity _ user <- handlerToWidget requireAuth
        messageRenderer <- getMessageRender
        $(widgetFile "admin-welcome")

    -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        auth <- requireAuth
        mCurrentR <- getCurrentRoute
        mmsg <- getMessage
        can <- getCan

        let am = filter (isJust . flip can "GET" . route) adminMenu
            mActiveMenuR = routeBestMatch mCurrentR $ map route am
            gravatarSize = 28 :: Int
            gOpts = def
                    { gSize = Just $ Size $ gravatarSize * 2 -- retina
                    }

        pc <- widgetToPageContent $ do
            addStylesheet $ coreR $ AdminStaticR $ CssAdminR NormalizeR
            addStylesheet $ coreR $ AdminStaticR $ CssAdminR BootstrapCssR
            addScript $ coreR $ AdminStaticR $ JsAdminR JQueryR
            addScript $ coreR $ AdminStaticR $ JsAdminR BootstrapJsR
            $(widgetFile "admin-layout")
        withUrlRenderer $(hamletFile "templates/admin-layout-wrapper.hamlet")

    adminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminAuthLayout widget = do
        mmsg <- getMessage
        logoRowId <- newIdent

        pc <- widgetToPageContent $ do
            addStylesheet $ coreR $ AdminStaticR $ CssAdminR NormalizeR
            addStylesheet $ coreR $ AdminStaticR $ CssAdminR BootstrapCssR
            addScript $ coreR $ AdminStaticR $ JsAdminR JQueryR
            addScript $ coreR $ AdminStaticR $ JsAdminR BootstrapJsR
            $(widgetFile "admin-auth-layout")
        withUrlRenderer $(hamletFile "templates/admin-auth-layout-wrapper.hamlet")

    authLogoR :: Route master
    authLogoR = coreR $ AdminStaticR $ ImageAdminR LambdaCmsLogoR
    -- | A list of menu items to show in the backend.
    -- Each site is different so what goes in the list should be provided by the Base app.
    --
    -- @
    -- [ MenuItem (SomeMessage MsgUser)    (UserAdminOverciewR)    "user"
    -- , MenuItem (SomeMessage MsgProduct) (ProductAdminOverviewR) "shopping-cart" ]
    -- @
    adminMenu :: [AdminMenuItem master]
    adminMenu = []

    -- | Renders a Core Message.
    renderCoreMessage :: master
                      -> [Text]
                      -> CoreMessage
                      -> Text
    renderCoreMessage m (lang:langs) = do
        case (lang `elem` (renderLanguages m), lang) of
            (True, "en") -> englishMessage
            (True, "nl") -> dutchMessage
            (True, "ru") -> russianMessage
            _ -> renderCoreMessage m langs
    renderCoreMessage _ _ = defaultMessage

    -- | A list of languages to render.
    renderLanguages :: master -> [Text]
    renderLanguages _ = ["en"]

    -- | A default way of (not) sending email: just print it to the stdout.
    -- See <https://github.com/lambdacms/lambdacms/blob/master/docs/implement-mail-deivery-method.md>
    -- for instructions on implementing a real delivery method.
    lambdaCmsSendMail :: Mail -> HandlerT master IO ()
    lambdaCmsSendMail (Mail from tos ccs bccs headers parts) =
        liftIO . putStrLn . unpack $ "MAIL"
            <> "\n  From: "        <> (address from)
            <> "\n  To: "          <> (maddress tos)
            <> "\n  Cc: "          <> (maddress ccs)
            <> "\n  Bcc: "         <> (maddress bccs)
            <> "\n  Subject: "     <> subject
            <> "\n  Attachment: "  <> attachment
            <> "\n  Plain body: "  <> plainBody
            <> "\n  Html body: "   <> htmlBody
        where
            subject = Data.Text.concat . map snd $ filter (\(k,_) -> k == "Subject") headers
            attachment :: Text
            attachment = intercalate ", " . catMaybes . map (partFilename) $
                concatMap (filter (isJust . partFilename)) parts
            htmlBody = getFromParts "text/html; charset=utf-8"
            plainBody = getFromParts "text/plain; charset=utf-8"
            getFromParts x = decodeUtf8 . LB.toStrict . LB.concat . map partContent $
                concatMap (filter ((==) x . partType)) parts
            maddress = intercalate ", " . map (address)
            address (Address n e) = let e' = "<" <> e <> ">" in case n of
                Just n' -> n' <> " " <> e'
                Nothing -> e'

-- | Ensures the admin user's lastLogin property is updated with logging in.
authenticateByLambdaCms :: LambdaCmsAdmin master
                        => Creds master
                        -> HandlerT master IO (AuthenticationResult master)
authenticateByLambdaCms creds = runDB $ do
    user <- getBy $ UniqueAuth (credsIdent creds) True
    case user of
        Just (Entity uid _) -> do
            timeNow <- liftIO getCurrentTime
            _ <- update uid [UserLastLogin =. Just timeNow]
            return $ Authenticated uid
        Nothing -> return $ UserError InvalidLogin

-- | Replace 'defaultMaybeAuthId' with this function to ensure (for every
-- request to the admin interface) that admin users are required to have
-- an active account.
-- TODO: Provide a bit more feedback then the standard 404 page when denying access.
lambdaCmsMaybeAuthId :: LambdaCmsAdmin master
                     => HandlerT master IO (Maybe (AuthId master))
lambdaCmsMaybeAuthId = do
    mauthId <- defaultMaybeAuthId
    maybe (return Nothing) maybeActiveAuthId mauthId
    where
        maybeActiveAuthId authId = do
            user <- runDB $ get404 authId
            return $ case userActive user of
                True -> Just authId
                False -> Nothing

-- | Checks whether a user is allowed perform an action and returns the route to that action if he is.
-- This way, users only see routes they're allowed to visit.
canFor :: LambdaCmsAdmin master
          => master                     -- Needed to make function injective.
          -> Maybe (Set (Roles master)) -- ^ Set of Roles the user has.
          -> Route master               -- ^ The action to perform.
          -> ByteString                 -- ^ The requested method (e/g: GET, POST, ...).
          -> Maybe (Route master)       -- ^ ust route when the user is allowed to perform the action, Nothing otherwise.
canFor m murs theRoute method = case isAuthorizedTo m murs $ actionAllowedFor theRoute method of
    Authorized -> Just theRoute
    _ -> Nothing

-- | A wrapper function that gets the roles of a user and calls 'canFor' with it.
-- This is what you'll use in a handler.
--
-- > can <- getCan
--
-- Then, in hamlet:
--
-- @
-- $maybe r <- can (SomeRouteR)
--   ... @{r}
-- @
getCan :: LambdaCmsAdmin master
       => HandlerT master IO (Route master -> ByteString -> Maybe (Route master))
getCan = do
    mauthId <- maybeAuthId
    murs <- forM mauthId getUserRoles
    y <- getYesod
    return $ canFor y murs

-- | A default admin menu.
defaultCoreAdminMenu :: LambdaCmsAdmin master
                     => (Route CoreAdmin -> Route master)
                     -> [AdminMenuItem master]
defaultCoreAdminMenu tp =
    [ MenuItem (SomeMessage Msg.MenuDashboard) (tp AdminHomeR) "home"
    , MenuItem (SomeMessage Msg.MenuUsers) (tp $ UserAdminR UserAdminIndexR) "user"
    ]

-- | Default admin layout.
--
-- Since 0.3.1.1
defaultAdminLayout :: ( Yesod s, YesodAuth s
                      , AuthId s ~ Key User, AuthEntity s ~ User
                      , LambdaCmsAdmin s)
                   => WidgetT s IO () -> HandlerT s IO Html
defaultAdminLayout widget = do
    auth      <- requireAuth
    mCurrentR <- getCurrentRoute
    mmsg      <- getMessage
    can       <- getCan

    let am = filter (isJust . flip can "GET" . route) adminMenu
        mActiveMenuR = routeBestMatch mCurrentR $ map route am
        gravatarSize = 28 :: Int
        gOpts = def { gSize = Just $ Size $ gravatarSize * 2 {- retina -} }

    pc <- widgetToPageContent $ do
        adminImportsWidget
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/admin-layout-wrapper.hamlet")

-- | Default admin layout for authorization.
--
-- Since 0.3.1.1
defaultAdminAuthLayout :: ( Yesod s, YesodAuth s
                          , AuthId s ~ Key User, AuthEntity s ~ User
                          , LambdaCmsAdmin s)
                       => WidgetT s IO () -> HandlerT s IO Html
defaultAdminAuthLayout widget = do
    mmsg      <- getMessage
    logoRowId <- newIdent

    pc <- widgetToPageContent $ do
        adminImportsWidget
        $(widgetFile "admin-auth-layout")
    withUrlRenderer $(hamletFile "templates/admin-auth-layout-wrapper.hamlet")

adminImportsWidget :: (Yesod s, LambdaCmsAdmin s)
                   => WidgetT s IO ()
adminImportsWidget = do
    addStylesheet . coreR $ AdminStaticR $ CssAdminR NormalizeR
    addStylesheet . coreR $ AdminStaticR $ CssAdminR BootstrapCssR
    addScript . coreR $ AdminStaticR $ JsAdminR JQueryR
    addScript . coreR $ AdminStaticR $ JsAdminR BootstrapJsR

-- | Shorcut for rendering a subsite Widget in the admin layout.
adminLayoutSub :: LambdaCmsAdmin master
                  => WidgetT sub IO ()
                  -> HandlerT sub (HandlerT master IO) Html
adminLayoutSub widget = widgetToParentWidget widget >>= lift . adminLayout

-- | Extension for bootstrap (give a name to input field).
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

withAttrs :: [(Text, Text)] -> FieldSettings site -> FieldSettings site
withAttrs attrs fs = fs { fsAttrs = attrs }

-- | Wrapper for humanReadableTimeI18N'. It uses Yesod's own i18n functionality.
lambdaCmsHumanTimeLocale :: LambdaCmsAdmin master
                         => HandlerT master IO HumanTimeLocale
lambdaCmsHumanTimeLocale = do
    langs <- languages
    y <- getYesod
    let rm = unpack . renderMessage y langs
    return $ HumanTimeLocale
        { justNow       = rm Msg.TimeJustNow
        , secondsAgo    = (\_ x -> rm . Msg.TimeSecondsAgo $ pack x)
        , oneMinuteAgo  = (\_   -> rm Msg.TimeOneMinuteAgo)
        , minutesAgo    = (\_ x -> rm . Msg.TimeMinutesAgo $ pack x)
        , oneHourAgo    = (\_   -> rm Msg.TimeOneHourAgo)
        , aboutHoursAgo = (\_ x -> rm . Msg.TimeAboutHoursAgo $ pack x)
        , at            = (\_ x -> rm $ Msg.TimeAt $ pack x)
        , daysAgo       = (\_ x -> rm . Msg.TimeDaysAgo $ pack x)
        , weekAgo       = (\_ x -> rm . Msg.TimeWeekAgo $ pack x)
        , weeksAgo      = (\_ x -> rm . Msg.TimeWeeksAgo $ pack x)
        , onYear        = rm . Msg.TimeOnYear . pack
        , locale        = lambdaCmsTimeLocale langs
        , dayOfWeekFmt  = rm Msg.DayOfWeekFmt
        , thisYearFmt   = "%b %e"
        , prevYearFmt   = "%b %e, %Y"
        , timeZone      = utc
        }

routeBestMatch :: RenderRoute master
                  => Maybe (Route master)
                  -> [Route master]
                  -> Maybe (Route master)
routeBestMatch (Just cr) rs = fmap snd $ find cmp orrs
    where
        (cparts, _) = renderRoute cr
        rrs = map ((fst . renderRoute) &&& id) rs
        orrs = reverse $ sortBy (comparing (length . fst)) rrs
        cmp (route', _) = route' == (take (length route') cparts)
routeBestMatch _ _ = Nothing

class LambdaCmsLoggable master entity where
    logMessage :: master -> ByteString -> entity -> [(Text, Text)]

instance LambdaCmsAdmin master => LambdaCmsLoggable master User where
    logMessage y "POST"       = translateUserLogs y Msg.LogCreatedUser
    logMessage y "PATCH"      = translateUserLogs y Msg.LogUpdatedUser
    logMessage y "DELETE"     = translateUserLogs y Msg.LogDeletedUser
    logMessage y "CHPASS"     = translateUserLogs y Msg.LogChangedPasswordUser
    logMessage y "RQPASS"     = translateUserLogs y Msg.LogRequestedPasswordUser
    logMessage y "DEACTIVATE" = translateUserLogs y Msg.LogDeactivatedUser
    logMessage y "ACTIVATE"   = translateUserLogs y Msg.LogActivatedUser
    logMessage _ _            = const []

translateUserLogs :: forall b master.
                     ( LambdaCmsAdmin master
                     , RenderMessage master b
                     ) => master -> (Text -> b) -> User -> [(Text, Text)]
translateUserLogs y msg u = map (id &&& messageFor) $ renderLanguages y
    where messageFor lang = renderMessage y [lang] . msg $ userName u

logUser :: LambdaCmsAdmin master => User -> HandlerT master IO [(Text, Text)]
logUser user = do
    y <- getYesod
    method <- waiRequest >>= return . requestMethod
    return $ logMessage y method user

logAction :: LambdaCmsAdmin master => [(Text, Text)] -> HandlerT master IO ()
logAction messages = do
    actionLogUserId    <- requireAuthId
    actionLogCreatedAt <- liftIO getCurrentTime
    actionLogIdent     <- liftIO generateUUID
    mapM_ (\(actionLogLang, actionLogMessage) -> runDB . insert_ $ ActionLog {..}) messages
