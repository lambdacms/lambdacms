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
  ( getUserAdminOverviewR
  , getUserAdminNewR
  , postUserAdminNewR
  , getUserAdminR
  , postUserAdminR
  , deleteUserAdminR
  , postUserAdminChangePasswordR
  ) where

import LambdaCms.Core.Import
import LambdaCms.Core.AuthHelper
import qualified Data.Text as T (breakOn, concat, length)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format.Human

-- data type for a form to change a user's password
data ComparePassword = ComparePassword { originalPassword :: Text
                                       , confirmPassword :: Text
                                       } deriving (Show, Eq)

getUserAdminOverviewR        :: CoreHandler Html
getUserAdminNewR             :: CoreHandler Html
postUserAdminNewR            :: CoreHandler Html
getUserAdminR                :: UserId -> CoreHandler Html
postUserAdminR               :: UserId -> CoreHandler Html
postUserAdminChangePasswordR :: UserId -> CoreHandler Html
deleteUserAdminR             :: UserId -> CoreHandler Html

userForm :: User -> Maybe Text -> Form User
userForm u submit = renderBootstrap3 BootstrapBasicForm $ User
             <$> pure            (userIdent u)
             <*> areq textField  (bfs' "username")        (Just $ userName u)
             <*> pure            (userPassword u)
             <*> areq emailField (bfs' "email address")   (Just $ userEmail u)
             <*> pure            (userToken u)
             <*> pure            (userCreatedAt u)
             <*> pure            (userLastLogin u)
             <*  bootstrapSubmit (bss submit)

userChangePasswordForm :: Maybe Text -> Maybe Text -> Form ComparePassword
userChangePasswordForm original submit = renderBootstrap3 BootstrapBasicForm $ ComparePassword
  <$> areq validatePasswordField (withName "original-pw" $ bfs' "password") Nothing
  <*> areq comparePasswordField  (bfs' "confirm") Nothing
  <*  bootstrapSubmit            (bss submit)
  where
    validatePasswordField = check validatePassword passwordField
    comparePasswordField = check comparePasswords passwordField

    validatePassword pw
      | T.length pw >= 8 = Right pw
      | otherwise = Left ("Password too short" :: Text)

    comparePasswords pw
      | pw == fromMaybe "" original = Right pw
      | otherwise = Left ("Passwords don't match" :: Text)


-- | Helper to create a user with email address
generateUserWithEmail :: Text -> IO User
generateUserWithEmail e = do
  uuid <- generateUUID
  timeNow <- getCurrentTime
  return $ User { userIdent     = uuid
                , userName      = fst $ T.breakOn "@" e
                , userPassword  = Nothing
                , userEmail     = e
                , userToken     = Nothing
                , userCreatedAt = timeNow
                , userLastLogin = timeNow
                }

-- | Helper to create an empty user
emptyUser :: IO User
emptyUser = generateUserWithEmail ""

getUserAdminOverviewR = do
    timeNow <- liftIO getCurrentTime
    (users :: [Entity User]) <- lift $ runDB $ selectList [] []
    lambdaCmsAdminLayoutSub $(whamletFile "templates/user/index.hamlet")

getUserAdminNewR = do
    tp <- getRouteToParent
    eu <- liftIO emptyUser
    (formWidget, enctype) <- lift . generateFormPost $ userForm eu (Just "Create")
    lambdaCmsAdminLayout $(whamletFile "templates/user/new.hamlet")

postUserAdminNewR = do
    eu <- liftIO emptyUser
    tp <- getRouteToParent
    ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm eu (Just "Create")
    case formResult of
      FormSuccess user -> do
        userId <- lift $ runDB $ insert user
        setMessage "successfully added"
        redirectUltDest $ UserAdminR userId
      _ -> do
        lambdaCmsAdminLayout $(whamletFile "templates/user/new.hamlet")

getUserAdminR userId = do
    tp <- getRouteToParent
    user <- lift $ runDB $ get404 userId
    (formWidget, enctype) <- lift . generateFormPost $ userForm user (Just "Save")
    (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just "Change")
    lambdaCmsAdminLayout $(whamletFile "templates/user/edit.hamlet")

postUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  tp <- getRouteToParent
  ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm user (Just "Save")
  (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just "Change")
  case formResult of
   FormSuccess updatedUser -> do
     _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
     setMessage "successfully replaced"
     redirect $ UserAdminR userId
   _ -> do
    lambdaCmsAdminLayout $(whamletFile "templates/user/edit.hamlet")

postUserAdminChangePasswordR userId = do
  user <- lift . runDB $ get404 userId
  tp <- getRouteToParent
  (formWidget, enctype) <- lift . generateFormPost $ userForm user (Just "Save")
  opw <- lookupPostParam "original-pw"
  ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $ userChangePasswordForm opw (Just "Change")
  case formResult of
   FormSuccess f -> do
     _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f)]
     setMessage "Successfully changed password"
     redirect $ UserAdminR userId
   _ -> do
     lambdaCmsAdminLayout $(whamletFile "templates/user/edit.hamlet")

deleteUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  lift . runDB $ delete userId
  setMessage . toHtml $ T.concat ["Deleted User: ", userName user]
  redirectUltDest UserAdminOverviewR
