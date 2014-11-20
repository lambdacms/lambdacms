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
import LambdaCms.Core.Message (CoreMessage)
import qualified LambdaCms.Core.Message as Msg
import LambdaCms.I18n

import Data.Time.Format
import qualified Data.Text as T (breakOn, concat, length)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format.Human
import System.Locale

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

userForm :: User -> Maybe CoreMessage -> Form User
userForm u submit = renderBootstrap3 BootstrapBasicForm $ User
             <$> pure            (userIdent u)
             <*> areq textField  (bfs Msg.Username)        (Just $ userName u)
             <*> pure            (userPassword u)
             <*> areq emailField (bfs Msg.EmailAddress)   (Just $ userEmail u)
             <*> pure            (userToken u)
             <*> pure            (userCreatedAt u)
             <*> pure            (userLastLogin u)
             <*  bootstrapSubmit (BootstrapSubmit (fromMaybe Msg.Submit submit) " btn-success " [])

userChangePasswordForm :: Maybe Text -> Maybe CoreMessage -> Form ComparePassword
userChangePasswordForm original submit = renderBootstrap3 BootstrapBasicForm $ ComparePassword
  <$> areq validatePasswordField (withName "original-pw" $ bfs Msg.Password) Nothing
  <*> areq comparePasswordField  (bfs Msg.Confirm) Nothing
  <*  bootstrapSubmit (BootstrapSubmit (fromMaybe Msg.Submit submit) " btn-success " [])
  where
    validatePasswordField = check validatePassword passwordField
    comparePasswordField = check comparePasswords passwordField

    validatePassword pw
      | T.length pw >= 8 = Right pw
      | otherwise = Left Msg.PasswordTooShort

    comparePasswords pw
      | pw == fromMaybe "" original = Right pw
      | otherwise = Left Msg.PasswordMismatch


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
  tp <- getRouteToParent
  timeNow <- liftIO getCurrentTime
  lift $ do
    hrtLocale <- lambdaCmsHumanTimeLocale
    (users :: [Entity User]) <- runDB $ selectList [] []
    adminLayout $ do
      setTitleI Msg.UserOverview
      $(whamletFile "templates/user/index.hamlet")

getUserAdminNewR = do
  tp <- getRouteToParent
  eu <- liftIO emptyUser
  lift $ do
    (formWidget, enctype) <- generateFormPost $ userForm eu (Just Msg.Create)
    adminLayout $ do
      setTitleI Msg.NewUser
      $(whamletFile "templates/user/new.hamlet")

postUserAdminNewR = do
    eu <- liftIO emptyUser
    ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm eu (Just Msg.Create)
    case formResult of
      FormSuccess user -> do
        userId <- lift $ runDB $ insert user
        lift $ setMessageI Msg.SuccessCreate
        redirectUltDest $ UserAdminR userId
      _ -> do
        tp <- getRouteToParent
        lift . adminLayout $ do
          setTitleI Msg.NewUser
          $(whamletFile "templates/user/new.hamlet")

getUserAdminR userId = do
    tp <- getRouteToParent
    timeNow <- liftIO getCurrentTime
    lift $ do
      user <- runDB $ get404 userId
      hrtLocale <- lambdaCmsHumanTimeLocale
      (formWidget, enctype) <- generateFormPost $ userForm user (Just Msg.Save)
      (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
      adminLayout $ do
        setTitleI . Msg.EditUser $ userName user
        $(whamletFile "templates/user/edit.hamlet")

postUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- lift lambdaCmsHumanTimeLocale
  ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm user (Just Msg.Save)
  (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
  case formResult of
   FormSuccess updatedUser -> do
     _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
     lift $ setMessageI Msg.SuccessReplace
     redirect $ UserAdminR userId
   _ -> do
     tp <- getRouteToParent
     lift . adminLayout $ do
       setTitleI . Msg.EditUser $ userName user
       $(whamletFile "templates/user/edit.hamlet")

postUserAdminChangePasswordR userId = do
  user <- lift . runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- lift lambdaCmsHumanTimeLocale
  (formWidget, enctype) <- lift . generateFormPost $ userForm user (Just Msg.Save)
  opw <- lookupPostParam "original-pw"
  ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $ userChangePasswordForm opw (Just Msg.Change)
  case formResult of
   FormSuccess f -> do
     _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f)]
     lift $ setMessageI Msg.SuccessChgPwd
     redirect $ UserAdminR userId
   _ -> do
     tp <- getRouteToParent
     lift . adminLayout $ do
       setTitleI . Msg.EditUser $ userName user
       $(whamletFile "templates/user/edit.hamlet")

deleteUserAdminR userId = do
  lift $ do
    user <- runDB $ get404 userId
    _ <- runDB $ delete userId
    setMessageI Msg.SuccessDelete
  redirectUltDest UserAdminOverviewR
