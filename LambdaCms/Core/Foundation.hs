{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ViewPatterns           #-}

module LambdaCms.Core.Foundation where

import Yesod
import Yesod.Auth
import Database.Persist.Sql (SqlBackend)

import           Yesod.Form.I18n.Dutch
import           Data.Monoid ((<>))
import           Data.Maybe (isJust, catMaybes)
import           Data.Text (Text, unpack, pack, intercalate, concat)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, toStrict)
import           Data.Time.Format.Human
import           Data.Time (utc)
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Message (CoreMessage, defaultMessage, englishMessage, dutchMessage)
import qualified LambdaCms.Core.Message as Msg
import           LambdaCms.I18n
import           Network.Mail.Mime

data Core = Core

mkYesodSubData "Core" $(parseRoutesFile "config/routes")

class ( YesodAuth master
      , AuthId master ~ Key User
      , AuthEntity master ~ User
      , YesodAuthPersist master
      , YesodPersistBackend master ~ SqlBackend
      ) => LambdaCmsAdmin master where

    coreR :: Route Core -> Route master
    authR :: Route Auth -> Route master
    -- | Gives the route which LambdaCms should use as the master site homepage
    masterHomeR :: Route master
    -- | Gives the route where to an unauthenticated user accessing the adminLayout should be redirected
    unauthenticatedR :: master -> Route master
    unauthenticatedR _ = authR LoginR

    -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        mauth <- maybeAuth
        case mauth of
          Just auth -> do
            cr <- getCurrentRoute
            mmsg <- getMessage
            let am = adminMenu
            pc <- widgetToPageContent $ do
              addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
              addScriptRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/js/bootstrap.min.js"
              addStylesheetRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/css/bootstrap.min.css"
              $(whamletFile "templates/adminlayout.hamlet")
              toWidget $(luciusFile "templates/adminlayout.lucius")
              toWidget $(juliusFile "templates/adminlayout.julius")
            withUrlRenderer $(hamletFile "templates/adminlayout-wrapper.hamlet")
          Nothing -> do
            y <- getYesod
            redirect $ unauthenticatedR y

    defaultLambdaCmsAdminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    defaultLambdaCmsAdminAuthLayout widget = do
        p <- widgetToPageContent $ do
            widget
            toWidget $(luciusFile "templates/adminauthlayout.lucius")
            toWidget $(juliusFile "templates/adminauthlayout.julius")
        mmsg <- getMessage
        withUrlRenderer $(hamletFile "templates/adminauthlayout.hamlet")

    isLoggedIn :: HandlerT master IO Bool
    isLoggedIn = do
        ma <- maybeAuthId
        return $ maybe False (const True) ma

    lambdaExtensions :: [LambdaCmsExtension master]
    lambdaExtensions = [] -- default to empty list to prevent runtime error (500)

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
                            { label :: SomeMessage master
                            , route :: Route master
                            , icon :: Text -- make this type-safe?
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
