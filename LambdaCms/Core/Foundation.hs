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
import           Database.Persist.Sql (SqlBackend)
import           Data.Text (Text)
import           Text.Hamlet (hamletFile)
import           Text.Lucius (luciusFile)
import           Text.Julius (juliusFile)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes

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
    adminMenu :: master -> [AdminMenuItem master]
    adminMenu _ = []

-- Fairly complex "handler" type, allowing persistent queries on the master's db connection, hereby simplified
type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT Core (HandlerT master IO) a

type CoreWidget = forall master. LambdaCmsAdmin master => WidgetT master IO ()

type Form x = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Fix for bfs (Bootstrap3 Field Settings)
bfs' :: Text -> FieldSettings master
bfs' = bfs . toMessage

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
