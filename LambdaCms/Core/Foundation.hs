{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {- # LANGUAGE FlexibleContexts      #-}
-- {- # LANGUAGE ConstraintKinds       #-}
-- {- # LANGUAGE UndecidableInstances  #-}

module LambdaCms.Core.Foundation where

import           Yesod
import           Data.Text (Text)

import           LambdaCms.Core.Models
import           LambdaCms.Core.Routes


class ( Yesod master
      , RenderRoute master
      , YesodPersist master
      --, PersistQuery (YesodPersistBackend master (HandlerT master IO))
      --, YesodPersistBackend master (HandlerT master IO)
      --, PersistMonadBackend (YesodPersistBackend master (HandlerT master IO))
      ) => LambdaCmsAdmin master where

    --runDB :: YesodPersistBackend master (HandlerT master IO) a -> HandlerT master IO a

       -- | Applies some form of layout to the contents of an admin section page.
    adminLayout :: WidgetT master IO () -> HandlerT master IO Html
    adminLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        user <- getUserName
        giveUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p} :: Admin
                    ^{pageHead p}
                <body>
                    <p>#{user}
                    $maybe msg <- mmsg
                        <p .message>#{msg}
                    <h1>Admin section
                    ^{pageBody p}
            |]

    defaultLambdaCmsAdminAuthLayout :: WidgetT master IO () -> HandlerT master IO Html
    defaultLambdaCmsAdminAuthLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        giveUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p} :: Admin Login
                    ^{pageHead p}
                <body>
                    $maybe msg <- mmsg
                        <p .message>#{msg}
                    <h1>Admin section
                    ^{pageBody p}
            |]

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


type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT Core (HandlerT master IO) a


type Form x = forall master. LambdaCmsAdmin master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
    renderMessage _ _ = defaultFormMessage
