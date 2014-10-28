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
import           Database.Persist.Sql (SqlBackend)
import           Data.Text (Text)
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes


class ( Yesod master
      , RenderRoute master
      , YesodPersist master
      ) => LambdaCmsAdmin master where

    --runDB :: YesodPersistBackend master (HandlerT master IO) a -> HandlerT master IO a

       -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout widget = do
        mmsg <- getMessage
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
            Just uid -> return . userIdent $ entityVal uid
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

-- Maybe place this in the LambdaCmsAdmin class if thats possible
lambdaCoreLayout :: CoreWidget -> CoreHandler Html
lambdaCoreLayout widget = do
  toParent <- getRouteToParent
  lift $ adminLayout $ do
    $(whamletFile "templates/adminmenu.hamlet")
    widget
