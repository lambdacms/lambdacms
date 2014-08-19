{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}

module LambdaCms.Core
    ( module LambdaCms.Core.Routes
    , module LambdaCms.Core
    ) where

import           Yesod
import           Data.Text (Text)
import           LambdaCms.Core.Routes


class (Yesod master, RenderRoute master) => LambdaCmsAdmin master where
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

    getUserName :: HandlerT master IO Text
    isLoggedIn :: HandlerT master IO Bool

type CoreHandler a = forall master. LambdaCmsAdmin master => HandlerT Core (HandlerT master IO) a

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift $ adminLayout [whamlet|Welcome to the admin section!|]

getAdminUsersR :: CoreHandler Html
getAdminUsersR = do
    users <- runDB $ selectList [] [] :: [Entity User]
    lift $ adminLayout [whamlet|Welcome to the admin users section! #{strU}|]


instance (Yesod master, LambdaCmsAdmin master) => YesodSubDispatch Core (HandlerT master IO) where
--instance LambdaCmsAdmin master => YesodSubDispatch Core (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCore)


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Core FormMessage where
    renderMessage _ _ = defaultFormMessage
