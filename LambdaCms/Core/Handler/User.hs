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
             <*> areq textField  (bfs MsgUsername)        (Just $ userName u)
             <*> pure            (userPassword u)
             <*> areq emailField (bfs MsgEmailAddress)   (Just $ userEmail u)
             <*> pure            (userToken u)
             <*> pure            (userCreatedAt u)
             <*> pure            (userLastLogin u)
             <*  bootstrapSubmit (BootstrapSubmit (fromMaybe MsgSubmit submit) " btn-success " [])

userChangePasswordForm :: Maybe Text -> Maybe CoreMessage -> Form ComparePassword
userChangePasswordForm original submit = renderBootstrap3 BootstrapBasicForm $ ComparePassword
  <$> areq validatePasswordField (withName "original-pw" $ bfs MsgPassword) Nothing
  <*> areq comparePasswordField  (bfs MsgConfirm) Nothing
  <*  bootstrapSubmit (BootstrapSubmit (fromMaybe MsgSubmit submit) " btn-success " [])
  where
    validatePasswordField = check validatePassword passwordField
    comparePasswordField = check comparePasswords passwordField

    validatePassword pw
      | T.length pw >= 8 = Right pw
      | otherwise = Left MsgPasswordTooShort

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
    hrtLocale <- lift lambdaCmsHumanTimeLocale
    (users :: [Entity User]) <- lift $ runDB $ selectList [] []
    lambdaCmsAdminLayoutSub $ do
      setTitle "User overview"
      $(whamletFile "templates/user/index.hamlet")

getUserAdminNewR = do
    eu <- liftIO emptyUser
    (formWidget, enctype) <- generateFormPost $ userForm eu (Just MsgCreate)
    lambdaCmsAdminLayoutSub $ do
      setTitle "New user"
      $(whamletFile "templates/user/new.hamlet")

postUserAdminNewR = do
    eu <- liftIO emptyUser
    ((formResult, formWidget), enctype) <- runFormPost $ userForm eu (Just MsgCreate)
    case formResult of
      FormSuccess user -> do
        userId <- lift $ runDB $ insert user
        setMessage "successfully added"
        redirectUltDest $ UserAdminR userId
      _ -> do
        lambdaCmsAdminLayoutSub $ do
          setTitle "New user"
          $(whamletFile "templates/user/new.hamlet")

getUserAdminR userId = do
    user <- lift $ runDB $ get404 userId
    (formWidget, enctype) <- generateFormPost $ userForm user (Just MsgSave)
    (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
    lambdaCmsAdminLayoutSub $ do
      setTitle . toHtml $ userName user
      $(whamletFile "templates/user/edit.hamlet")

postUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  ((formResult, formWidget), enctype) <- runFormPost $ userForm user (Just MsgSave)
  (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
  case formResult of
   FormSuccess updatedUser -> do
     _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
     setMessage "successfully replaced"
     redirect $ UserAdminR userId
   _ -> do
    lambdaCmsAdminLayoutSub $ do
      setTitle . toHtml $ userName user
      $(whamletFile "templates/user/edit.hamlet")

postUserAdminChangePasswordR userId = do
  user <- lift . runDB $ get404 userId
  (formWidget, enctype) <- generateFormPost $ userForm user (Just MsgSave)
  opw <- lookupPostParam "original-pw"
  ((formResult, pwFormWidget), pwEnctype) <- runFormPost $ userChangePasswordForm opw (Just MsgChange)
  case formResult of
   FormSuccess f -> do
     _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f)]
     setMessage "Successfully changed password"
     redirect $ UserAdminR userId
   _ -> do
     lambdaCmsAdminLayoutSub $ do
       setTitle . toHtml $ userName user
       $(whamletFile "templates/user/edit.hamlet")

deleteUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  lift . runDB $ delete userId
  setMessage . toHtml $ T.concat ["Deleted User: ", userName user]
  redirectUltDest UserAdminOverviewR
