{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}


module LambdaCms.Core.Handler.User
  ( getUserAdminR
  , getUserAdminNewR
  , postUserAdminNewR
  , getUserAdminDetailR
  , getUserAdminEditR
  , postUserAdminEditR
  , postUserAdminDeleteR
  ) where

import LambdaCms.Core.Import

-- getHomeR = do
--     (formWidget, formEnctype) <- generateFormPost sampleForm
--     let submission = Nothing :: Maybe (FileInfo, Text)
--         handlerName = "getHomeR" :: Text
--     adminLayout $ do
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- getUserAdminsR :: CoreHandler Html
-- getUserAdminsR =
--     do users <- lift $ runDB $ selectList [] [] :: [Entity User]
--        let strU = show users
--        lift $ adminLayout [whamlet|Welcome to the admin users section! #{strU}|]


-- import Import
-- import Widgets
-- import qualified Data.Text as DT

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderDivs $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField "What's on the file?" Nothing

-- userForm :: Maybe User -> Form User
-- userForm mUser = renderDivs $ User
--     <$> areq textField   "ident" (userIdent <$> mUser)
    -- <*> areq textField   "pass"  (userPassword <$> mUser)


getUserAdminR        :: CoreHandler Html
getUserAdminNewR     :: CoreHandler Html
postUserAdminNewR    :: CoreHandler Html
getUserAdminDetailR  :: UserId -> CoreHandler Html
getUserAdminEditR    :: UserId -> CoreHandler Html
postUserAdminEditR   :: UserId -> CoreHandler Html
postUserAdminDeleteR :: UserId -> CoreHandler Html

getUserAdminR = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- (allUsers :: [Entity User]) <- runDB $ selectList [] []
  -- adminLayout $(widgetFile "user/index")

getUserAdminNewR = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- (formWidget, enctype) <- generateFormPost $ userForm Nothing
  -- adminLayout $(widgetFile "user/new")

postUserAdminNewR = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- ((formResult, _), _) <- runFormPost $ userForm Nothing
  -- case formResult of
  --   FormSuccess user -> do
  --     userId <- runDB $ insert user
  --     setMessage "successfully added"
  --     redirectUltDest $ UserAdminDetailR userId
  --   _ -> do
  --     setMessage "form error"
  --     redirectUltDest UserAdminNewR

getUserAdminDetailR userId = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- user <- runDB $ get404 userId
  -- adminLayout $(widgetFile "user/show")

getUserAdminEditR userId = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- user <- runDB $ get404 userId
  -- (formWidget, enctype) <- generateFormPost $ userForm (Just user)
  -- adminLayout $(widgetFile "user/edit")

postUserAdminEditR userId = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- ((formResult, _), _) <- runFormPost $ userForm Nothing
  -- case formResult of
  --   FormSuccess user -> do
  --     _ <- runDB $ replace userId user
  --     setMessage "successfully replaced"
  --   _ -> setMessage "form error"
  -- redirectUltDest $ UserAdminDetailR userId

postUserAdminDeleteR userId = do
    lift $ adminLayout [whamlet|Welcome to the admin's user section!|]
  -- _ <- runDB $ get404 userId
  -- runDB $ delete userId
  -- setMessage . toHtml $ DT.concat ["Deleted User with id: ", toPathPiece userId]
  -- redirectUltDest UserAdminR
