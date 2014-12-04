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

data Core = Core

mkYesodSubData "Core" $(parseRoutesFile "config/routes")

data Allow a = Unauthenticated -- ^ Allow anyone (no authentication required)
             | Authenticated   -- ^ Allow any authenticated user
             | Roles a         -- ^ Allow anyone who as at least one matching role
             | Nobody          -- ^ Allow nobody

canFor :: LambdaCmsAdmin master
          => master -- Needed to make function injective
          -> Maybe (Set (Roles master))
          -> Route master
          -> ByteString
          -> Maybe (Route master)
canFor m murs theRoute method = case isAuthorizedTo m murs $ actionAllowedFor theRoute method of
    Authorized -> Just theRoute
    _ -> Nothing

getCan :: LambdaCmsAdmin master => HandlerT master IO (Route master -> ByteString -> Maybe (Route master))
getCan = do
    mauthId <- maybeAuthId
    murs <- forM mauthId getUserRoles
    y <- getYesod
    return $ canFor y murs

class ( YesodAuth master
      , AuthId master ~ Key User
      , AuthEntity master ~ User
      , YesodAuthPersist master
      , YesodPersistBackend master ~ SqlBackend
      , Ord (Roles master)
      , Enum (Roles master)
      , Bounded (Roles master)
      , Show (Roles master)
      , Eq (Roles master)
      ) => LambdaCmsAdmin master where

    type Roles master

    getUserRoles :: Key User -> HandlerT master IO (Set (Roles master))
    setUserRoles :: Key User -> Set (Roles master) -> HandlerT master IO ()

    -- | See if a user is authorized to perform an action.
    isAuthorizedTo :: master -- Needed to make function injective
                   -> Maybe (Set (Roles master))
                   -> Allow (Set (Roles master)) -- ^ Set of roles allowed to perform the action
                   -> AuthResult
    isAuthorizedTo _ _           Nobody          = Unauthorized "Access denied."
    isAuthorizedTo _ _           Unauthenticated = Authorized
    isAuthorizedTo _ (Just _)    Authenticated   = Authorized
    isAuthorizedTo _ Nothing     _               = AuthenticationRequired
    isAuthorizedTo _ (Just urs) (Roles rrs)    = do
      case (not . S.null $ urs `intersection` rrs) of
        True -> Authorized
        False -> Unauthorized "Access denied."

    actionAllowedFor :: Route master -> ByteString -> Allow (Set (Roles master))
    actionAllowedFor _ _ = Nobody

    coreR :: Route Core -> Route master
    authR :: Route Auth -> Route master
    -- | Gives the route which LambdaCms should use as the master site homepage
    masterHomeR :: Route master

    -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        mauth <- maybeAuth
        case mauth of
          Just auth -> do
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
          Nothing -> do
            y <- getYesod
            case authRoute y of
              Just route -> redirect route
              Nothing -> notAuthenticated

    defaultLambdaCmsAdminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    defaultLambdaCmsAdminAuthLayout widget = do
        p <- widgetToPageContent $ do
            widget
            toWidget $(luciusFile "templates/adminauthlayout.lucius")
            toWidget $(juliusFile "templates/adminauthlayout.julius")
        mmsg <- getMessage
        withUrlRenderer $(hamletFile "templates/adminauthlayout.hamlet")

    adminMenu :: [AdminMenuItem master]
    adminMenu = []

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

    renderLanguages :: master -> [Text]
    renderLanguages _ = ["en"]

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


-- Fairly complex "handler" type, allowing persistent queries on the master's db connection, hereby simplified
type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT Core (HandlerT master IO) a

type CoreForm a = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult a, WidgetT master IO ())

instance LambdaCmsAdmin master => RenderMessage master CoreMessage where
  renderMessage = renderCoreMessage

-- Extension for bootstrap (give a name to input field)
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

data AdminMenuItem master = MenuItem
                            { label :: SomeMessage master
                            , route :: Route master
                            , icon  :: Text -- make this type-safe?
                            }

defaultCoreAdminMenu :: LambdaCmsAdmin master => (Route Core -> Route master) -> [AdminMenuItem master]
defaultCoreAdminMenu tp = [MenuItem (SomeMessage Msg.MenuDashboard) (tp AdminHomeR) "home",
                           MenuItem (SomeMessage Msg.MenuUsers) (tp $ UserAdminR UserAdminIndexR) "user"]


adminLayoutSub :: LambdaCmsAdmin master
                  => WidgetT sub IO ()
                  -> HandlerT sub (HandlerT master IO) Html
adminLayoutSub widget = widgetToParentWidget widget >>= lift . adminLayout


-- | Wrapper for humanReadableTimeI18N'. It uses Yesod's own i18n functionality
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
