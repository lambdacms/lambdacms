{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}

module LambdaCms.Core.Foundation where

import           Yesod
import           Yesod.Form.Bootstrap3
import           Database.Persist.Sql (SqlBackend)
import           Data.Text (Text)
import           Data.Maybe (fromMaybe)
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes


class ( Yesod master
      , RenderRoute master
      , YesodPersist master
      , RenderMessage master FormMessage
      ) => LambdaCmsAdmin master where

    --runDB :: YesodPersistBackend master (HandlerT master IO) a -> HandlerT master IO a

       -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
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

-- Fairly complex "handler" type, allowing persistent queries on the master's db connection, hereby simplified
type CoreHandler a = forall master.
    ( LambdaCmsAdmin master
    , PersistQuery (YesodPersistBackend master)
    , YesodPersistBackend master ~ SqlBackend
    ) => HandlerT Core (HandlerT master IO) a

type CoreWidget = forall master.
    ( LambdaCmsAdmin master
    , PersistQuery (YesodPersistBackend master)
    , YesodPersistBackend master ~ SqlBackend
    ) => WidgetT master IO ()

type Form x = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Fix for bfs (Bootstrap3 Field Settings)
bfs' :: Text -> FieldSettings master
bfs' = bfs . toMessage

-- Wrapper around BootstrapSubmit
bss :: Maybe Text -> BootstrapSubmit Text
bss submit = (BootstrapSubmit (fromMaybe "Submit" submit) " btn-success " [])

-- Extension for bootstrap (give a name to input field)
withName :: Text -> FieldSettings site -> FieldSettings site
withName name fs = fs { fsName = Just name }

-- Maybe place this in the LambdaCmsAdmin class if thats possible
lambdaCoreLayout :: forall master.
                    LambdaCmsAdmin master
                    => WidgetT master IO ()
                    -> HandlerT Core (HandlerT master IO) Html
lambdaCoreLayout widget = do
  toParent <- getRouteToParent
  curR <- lift getCurrentRoute
  mmsg <- getMessage
  lift $ adminLayout $ do
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"
    addScriptRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/js/bootstrap.min.js"
    addStylesheetRemote "//cdn.jsdelivr.net/bootstrap/3.3.0/css/bootstrap.min.css"
    $(whamletFile "templates/lambdacorelayout.hamlet")
