{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module LambdaCms.Core.Foundation where

import           Control.Monad              (filterM)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, toStrict)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Monoid                ((<>))
import           Data.Set                   (Set, fromList, intersection)
import qualified Data.Set                   as S (null)
import           Data.Text                  (Text, concat, intercalate, pack,
                                             unpack)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time                  (utc)
import           Data.Time.Format.Human
import           Data.Traversable           (forM)
import           Database.Persist.Sql       (SqlBackend)
import           LambdaCms.Core.Message     (CoreMessage, defaultMessage,
                                             dutchMessage, englishMessage)
import qualified LambdaCms.Core.Message     as Msg
import           LambdaCms.Core.Models
import           LambdaCms.Core.Settings
import           LambdaCms.I18n
import           Network.Gravatar           (GravatarOptions (..), Size (..),
                                             def, gravatar)
import           Network.Mail.Mime
import           Text.Hamlet                (hamletFile)
import           Text.Julius                (juliusFile)
import           Text.Lucius                (luciusFile)
import           Yesod
import           Yesod.Auth

-- | Foundation type.
data CoreAdmin = CoreAdmin

-- | Denotes what kind of user is allowed to perform an action.
data Allow a = Unauthenticated -- ^ Allow anyone (no authentication required).
             | Authenticated   -- ^ Allow any authenticated user.
             | Roles a         -- ^ Allow anyone who as at least one matching role.
             | Nobody          -- ^ Allow nobody.

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

-- | Fairly complex "handler" type, allowing persistent queries on the master's db connection, hereby simplified.
type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT CoreAdmin (HandlerT master IO) a

-- | Fairly complex Form type, hereby simplified.
type CoreForm a = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult a, WidgetT master IO ())

class ( YesodAuth master
      , AuthId master ~ Key User
      , AuthEntity master ~ User
      , YesodAuthPersist master
      , YesodPersistBackend master ~ SqlBackend
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

    -- | See if a user is authorized to perform an action.
    isAuthorizedTo :: master                     -- Needed to make function injective.
                   -> Maybe (Set (Roles master)) -- ^ Set of roles the user has.
                   -> Allow (Set (Roles master)) -- ^ Set of roles allowed to perform the action.
                   -> AuthResult
    isAuthorizedTo _ _           Nobody          = Unauthorized "Access denied."
    isAuthorizedTo _ _           Unauthenticated = Authorized
    isAuthorizedTo _ (Just _)    Authenticated   = Authorized
    isAuthorizedTo _ Nothing     _               = AuthenticationRequired
    isAuthorizedTo _ (Just urs) (Roles rrs)    = do
      case (not . S.null $ urs `intersection` rrs) of
        True -> Authorized -- non-empty intersection means authorized
        False -> Unauthorized "Access denied."

    -- | Get the 'Allow' type needed for this action.
    -- The default is that no one can do anything.
    actionAllowedFor :: Route master -- ^ The action (or route).
                     -> ByteString -- ^ The request method (e/g: GET, POST, DELETE, ...).
                                   -- Knowing /which/ method is used allows for more fine grained
                                   -- permissions than only knowing whether it is /write/ request.
                     -> Allow (Set (Roles master))
    actionAllowedFor _ _ = Nobody

    -- | Both coreR and authR are used to navigate to a different controller.
    -- It saves you from putting "getRouteToParent" everywhere.
    coreR :: Route CoreAdmin -> Route master
    authR :: Route Auth -> Route master

    -- | Gives the route which LambdaCms should use as the master site homepage.
    masterHomeR :: Route master

    -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        auth <- requireAuth
        cr <- getCurrentRoute
        mmsg <- getMessage
        can <- getCan

        let am = filter (isJust . flip can "GET" . route) adminMenu
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

    defaultLambdaCmsAdminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    defaultLambdaCmsAdminAuthLayout widget = do
        p <- widgetToPageContent $ do
            widget
            toWidget $(luciusFile "templates/adminauthlayout.lucius")
            toWidget $(juliusFile "templates/adminauthlayout.julius")
        mmsg <- getMessage
        withUrlRenderer $(hamletFile "templates/adminauthlayout.hamlet")

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
            _ -> renderCoreMessage m langs
    renderCoreMessage _ _ = defaultMessage

    -- | A list of languages to render.
    renderLanguages :: master -> [Text]
    renderLanguages _ = ["en"]

    -- | A default way of sending email. See <https://github.com/lambdacms/lambdacms-core/blob/master/sending-emails.md github> for details.
    -- The default is to print it all to stdout.
    lambdaCmsSendMail :: master -> Mail -> IO ()
    lambdaCmsSendMail _ (Mail from tos ccs bccs headers parts) =
        putStrLn . unpack $
        "MAIL"
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
            attachment = intercalate ", " . catMaybes . map (partFilename) $ concatMap (filter (isJust . partFilename)) parts
            htmlBody = getFromParts "text/html; charset=utf-8"
            plainBody = getFromParts "text/plain; charset=utf-8"
            getFromParts x = decodeUtf8 . LB.toStrict . LB.concat . map partContent $ concatMap (filter ((==) x . partType)) parts
            maddress = intercalate ", " . map (address)
            address (Address n e) = case n of
                                        Just n' -> n' <> " " <> e'
                                        Nothing -> e'
                where e' = "<" <> e <> ">"

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
getCan :: LambdaCmsAdmin master => HandlerT master IO (Route master -> ByteString -> Maybe (Route master))
getCan = do
    mauthId <- maybeAuthId
    murs <- forM mauthId getUserRoles
    y <- getYesod
    return $ canFor y murs

-- | A default admin menu.
defaultCoreAdminMenu :: LambdaCmsAdmin master => (Route CoreAdmin -> Route master) -> [AdminMenuItem master]
defaultCoreAdminMenu tp = [ MenuItem (SomeMessage Msg.MenuDashboard) (tp AdminHomeR) "home"
                          , MenuItem (SomeMessage Msg.MenuUsers) (tp $ UserAdminR UserAdminIndexR) "user"
                          ]

-- | Shorcut for rendering a subsite Widget in the admin layout.
adminLayoutSub :: LambdaCmsAdmin master
                  => WidgetT sub IO ()
                  -> HandlerT sub (HandlerT master IO) Html
adminLayoutSub widget = widgetToParentWidget widget >>= lift . adminLayout

-- | Extension for bootstrap (give a name to input field).
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

-- | Wrapper for humanReadableTimeI18N'. It uses Yesod's own i18n functionality.
lambdaCmsHumanTimeLocale :: LambdaCmsAdmin master => HandlerT master IO HumanTimeLocale
lambdaCmsHumanTimeLocale = do
    langs <- languages
    y <- getYesod
    let rm = unpack . renderMessage y langs
    return $ HumanTimeLocale
        { justNow       = rm Msg.TimeJustNow
        , secondsAgo    = rm . Msg.TimeSecondsAgo . pack
        , oneMinuteAgo  = rm Msg.TimeOneMinuteAgo
        , minutesAgo    = rm . Msg.TimeMinutesAgo . pack
        , oneHourAgo    = rm Msg.TimeOneHourAgo
        , aboutHoursAgo = rm . Msg.TimeAboutHoursAgo . pack
        , at            = (\_ x -> rm $ Msg.TimeAt $ pack x)
        , daysAgo       = rm . Msg.TimeDaysAgo . pack
        , weekAgo       = rm . Msg.TimeWeekAgo . pack
        , weeksAgo      = rm . Msg.TimeWeeksAgo . pack
        , onYear        = rm . Msg.TimeOnYear . pack
        , locale        = lambdaCmsTimeLocale langs
        , timeZone      = utc
        , dayOfWeekFmt  = rm Msg.DayOfWeekFmt
        , thisYearFmt   = "%b %e"
        , prevYearFmt   = "%b %e, %Y"
        }
