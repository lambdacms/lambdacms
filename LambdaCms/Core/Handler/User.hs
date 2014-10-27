{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

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


getUserAdminR        :: CoreHandler Html
getUserAdminNewR     :: CoreHandler Html
postUserAdminNewR    :: CoreHandler Html
getUserAdminDetailR  :: UserId -> CoreHandler Html
getUserAdminEditR    :: UserId -> CoreHandler Html
postUserAdminEditR   :: UserId -> CoreHandler Html
postUserAdminDeleteR :: UserId -> CoreHandler Html

getUserAdminR = do
    (allUsers :: [Entity User]) <- lift $ runDB $ selectList [] []
    let a = show allUsers
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/index.hamlet")

getUserAdminNewR = do
  -- (formWidget, enctype) <- generateFormPost $ userForm Nothing
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/new.hamlet")

postUserAdminNewR = do
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/show.hamlet")
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
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/show.hamlet")
  -- user <- runDB $ get404 userId
  -- adminLayout $(widgetFile "user/show")

getUserAdminEditR userId = do
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/edit.hamlet")
  -- user <- runDB $ get404 userId
  -- (formWidget, enctype) <- generateFormPost $ userForm (Just user)
  -- adminLayout $(widgetFile "user/edit")

postUserAdminEditR userId = do
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/show.hamlet")
  -- ((formResult, _), _) <- runFormPost $ userForm Nothing
  -- case formResult of
  --   FormSuccess user -> do
  --     _ <- runDB $ replace userId user
  --     setMessage "successfully replaced"
  --   _ -> setMessage "form error"
  -- redirectUltDest $ UserAdminDetailR userId

postUserAdminDeleteR userId = do
    toParent <- getRouteToParent
    lift $ adminLayout $ do
      lambdaAdminMenuWidget toParent
      $(whamletFile "templates/user/show.hamlet")
  -- _ <- runDB $ get404 userId
  -- runDB $ delete userId
  -- setMessage . toHtml $ DT.concat ["Deleted User with id: ", toPathPiece userId]
  -- redirectUltDest UserAdminR
