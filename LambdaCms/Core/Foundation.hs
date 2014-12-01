{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}

module LambdaCms.Core.Foundation where

import           Yesod
import           Yesod.Form.I18n.Dutch
import           Database.Persist.Sql (SqlBackend)
import           Data.Monoid ((<>))
import           Data.Maybe (isJust, catMaybes)
import qualified Data.List as L (intersect)
import           Data.Text (Text, unpack, pack, intercalate, concat)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, toStrict)
import           Data.Time.Format.Human
import           Data.Time (utc)
import qualified Data.Set as S
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes
import           LambdaCms.Core.Message (CoreMessage, defaultMessage)
import qualified LambdaCms.Core.Message as Msg
import           LambdaCms.I18n
import           Network.Mail.Mime

-- | A type to determine what kind of user is allowed to do something.
data Allow a = Unauthenticated -- ^ Allow anyone (no authentication required)
             | Authenticated   -- ^ Allow any authenticated user
             | Roles a         -- ^ Allow anyone who as at least one matching role
             | Nobody          -- ^ Allow nobody

class ( Yesod master
      -- , YesodDispatch master
      , RenderRoute master
      , RenderMessage master FormMessage
      , YesodPersist master
      , YesodPersistBackend master ~ SqlBackend
      , Ord (Roles master)
      -- , PersistQuery (YesodPersistBackend master)
      ) => LambdaCmsAdmin master where
    --runDB :: YesodPersistBackend master (HandlerT master IO) a -> HandlerT master IO a

    type Roles master

    getUserRoles :: Entity User -> YesodDB master (S.Set (Roles master))
    setUserRoles :: Entity User -> S.Set (Roles master) -> YesodDB master ()

    -- | See if a user is authorized to perform an action.
    isAuthorizedTo
      :: Ord (Roles master)
      => Maybe (Entity User)
      -> Allow (S.Set (Roles master)) -- ^ Set of roles allowed to perform the action
      -> YesodDB master AuthResult
    isAuthorizedTo _           Nobody          = return $ Unauthorized "Access denied."
    isAuthorizedTo _           Unauthenticated = return Authorized
    isAuthorizedTo (Just _)    Authenticated   = return Authorized
    isAuthorizedTo Nothing     _               = return AuthenticationRequired
    isAuthorizedTo (Just user) (Roles xs)      = do
      ur <- getUserRoles user
      case (not . S.null $ ur `S.intersection` xs) of
        True -> return Authorized
        False -> return $ Unauthorized "Access denied."

    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        y <- getYesod
        let mis = adminMenu y
        cr <- getCurrentRoute
        un <- getUserName
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
          addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
          addScriptRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/js/bootstrap.min.js"
          addStylesheetRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/css/bootstrap.min.css"
          $(whamletFile "templates/adminlayout.hamlet")
          toWidget $(luciusFile "templates/adminlayout.lucius")
          toWidget $(juliusFile "templates/adminlayout.julius")
        withUrlRenderer $(hamletFile "templates/adminlayout-wrapper.hamlet")

    defaultLambdaCmsAdminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    defaultLambdaCmsAdminAuthLayout widget = do
        p <- widgetToPageContent $ do
            widget
            toWidget $(luciusFile "templates/adminauthlayout.lucius")
            toWidget $(juliusFile "templates/adminauthlayout.julius")
        mmsg <- getMessage
        withUrlRenderer $(hamletFile "templates/adminauthlayout.hamlet")

    maybeAuth' :: HandlerT master IO (Maybe (Entity User))
    maybeAuthId' :: HandlerT master IO (Maybe UserId)
    authLoginDest :: master -> Route master
    authLogoutRoute :: master -> Route master
    masterHomeRoute :: master -> Route master

    getUserName :: HandlerT master IO Text
    getUserName = do
        y <- getYesod
        muid <- maybeAuth'
        case muid of
            Nothing -> do
                setMessage "Not logged in"
                redirect $ authLoginDest y
            Just uid -> return . userEmail $ entityVal uid

    isLoggedIn :: HandlerT master IO Bool
    isLoggedIn = do
        ma <- maybeAuthId'
        return $ maybe False (const True) ma

    lambdaExtensions :: master -> [LambdaCmsExtension master]
    lambdaExtensions _ = [] -- default to empty list to prevent runtime error (500)

    adminMenu :: master -> [AdminMenuItem master]
    adminMenu _ = []

    renderCoreMessage :: master
                         -> [Text]
                         -> CoreMessage
                         -> Text
    renderCoreMessage _ _ = defaultMessage

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

type CoreWidget = forall master. LambdaCmsAdmin master => WidgetT master IO ()

type Form x = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
  renderMessage _ _ = dutchFormMessage

instance LambdaCmsAdmin master => RenderMessage master CoreMessage where
  renderMessage = renderCoreMessage

-- Extension for bootstrap (give a name to input field)
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

data LambdaCmsExtension master = LambdaCmsExtension
                                 { extensionName :: Text
                                 , extensionMenuItem :: Maybe (AdminMenuItem master)
                                 --, contentTypes
                                 --, adminComponents
                                 }

data AdminMenuItem master = MenuItem
                            { label :: Text
                            , route :: Route master
                            , icon :: Text -- make this type-safe?
                            }

defaultCoreAdminMenu :: LambdaCmsAdmin master => (Route Core -> Route master) -> [AdminMenuItem master]
defaultCoreAdminMenu tp = [MenuItem "Dashboard" (tp AdminHomeR) "home",
                           MenuItem "Users" (tp UserAdminOverviewR) "user"]


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
