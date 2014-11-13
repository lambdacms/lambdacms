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
import           Yesod.Form.Bootstrap3
import           Yesod.Form.I18n.Dutch
import           Database.Persist.Sql (SqlBackend)
import           Data.Text (Text, unpack)
import           Data.Time.Format.Human
import           Data.Time (utc)
import           System.Locale
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes
import           LambdaCms.I18n

mkMessage "Core" "messages" "en"

class ( Yesod master
      -- , YesodDispatch master
      , RenderRoute master
      , RenderMessage master FormMessage
      , YesodPersist master
      , YesodPersistBackend master ~ SqlBackend
      -- , PersistQuery (YesodPersistBackend master)
      ) => LambdaCmsAdmin master where

    --runDB :: YesodPersistBackend master (HandlerT master IO) a -> HandlerT master IO a

       -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        y <- getYesod
        user <- getUserName
        p <- widgetToPageContent $ do
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

-- Fairly complex "handler" type, allowing persistent queries on the master's db connection, hereby simplified
type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT Core (HandlerT master IO) a

type CoreWidget = forall master. LambdaCmsAdmin master => WidgetT master IO ()

type Form x = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT Core (HandlerT master IO)) (FormResult x, WidgetT Core IO ())


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
    renderMessage _ _ = dutchFormMessage

-- Fix for bfs (Bootstrap3 Field Settings)
bfs' :: Text -> FieldSettings master
bfs' = bfs . toMessage

-- Wrapper around BootstrapSubmit
bss :: Maybe Text -> BootstrapSubmit Text
bss submit = (BootstrapSubmit (fromMaybe "Submit" submit) " btn-success " [])

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

lambdaCmsAdminLayout :: LambdaCmsAdmin parent =>
                        WidgetT parent IO () ->
                        HandlerT child (HandlerT parent IO) Html
lambdaCmsAdminLayout widget = lift $ do
  y  <- getYesod
  let mis = adminMenu y
  cr <- getCurrentRoute
  un <- getUserName
  mmsg <- getMessage
  pc <- widgetToPageContent $ do
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
    addScriptRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/js/bootstrap.min.js"
    addStylesheetRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/css/bootstrap.min.css"
    $(whamletFile "templates/lambdacmsadminlayout.hamlet")
    toWidget $(luciusFile "templates/adminlayout.lucius")
    toWidget $(juliusFile "templates/adminlayout.julius")
  withUrlRenderer $(hamletFile "templates/lambdacmsadminlayout-wrapper.hamlet")

lambdaCmsAdminLayoutSub :: LambdaCmsAdmin parent =>
                           WidgetT child IO () ->
                           HandlerT child (HandlerT parent IO) Html
lambdaCmsAdminLayoutSub cwidget = widgetToParentWidget cwidget >>= lambdaCmsAdminLayout


-- | Wrapper for humanReadableTimeI18N'. It uses Yesod's own i18n functionality
lambdaCmsHumanTimeLocale :: MonadHandler m => m HumanTimeLocale
lambdaCmsHumanTimeLocale = do
  langs <- languages
  let rm = unpack . renderMessage Core langs
  return $ HumanTimeLocale
    { justNow       = rm MsgTimeJustNow
    , secondsAgo    = rm . MsgTimeSecondsAgo
    , oneMinuteAgo  = rm MsgTimeOneMinuteAgo
    , minutesAgo    = rm . MsgTimeMinutesAgo
    , oneHourAgo    = rm MsgTimeOneHourAgo
    , aboutHoursAgo = rm . MsgTimeAboutHoursAgo
    , at            = (\_ x -> rm $ MsgTimeAt x)
    , daysAgo       = rm . MsgTimeDaysAgo
    , weekAgo       = rm . MsgTimeWeekAgo
    , weeksAgo      = rm . MsgTimeWeeksAgo
    , onYear        = rm . MsgTimeOnYear
    , locale        = lambdaCmsTimeLocale langs
    , timeZone      = utc
    , dayOfWeekFmt  = rm MsgDayOfWeekFmt
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }
