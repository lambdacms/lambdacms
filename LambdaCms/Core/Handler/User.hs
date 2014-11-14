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
  , getUserAdminActivateR
  , postUserAdminActivateR
  ) where

import LambdaCms.Core.Import
import LambdaCms.Core.AuthHelper
import LambdaCms.I18n
import Data.Time.Format
import qualified Data.Text as T (breakOn, concat, length)
import qualified Data.Text.Lazy as LT (Text)

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format.Human
import System.Locale
import Network.Mail.Mime
import Text.Blaze.Renderer.Text  (renderHtml)

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
getUserAdminActivateR        :: UserId -> Text -> CoreHandler Html
postUserAdminActivateR        :: UserId -> Text -> CoreHandler Html

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
      | otherwise = Left MsgPasswordMismatch


-- | Helper to create a user with email address
generateUserWithEmail :: Text -> IO User
generateUserWithEmail e = do
  uuid <- generateUUID
  token <- generateActivationToken
  timeNow <- getCurrentTime
  return $ User { userIdent     = uuid
                , userName      = fst $ T.breakOn "@" e
                , userPassword  = Nothing
                , userEmail     = e
                , userToken     = Just token
                , userCreatedAt = timeNow
                , userLastLogin = timeNow
                }

-- | Helper to create an empty user
emptyUser :: IO User
emptyUser = generateUserWithEmail ""

-- | Validate an activation token
validateUserToken :: User -> Text -> Maybe Bool
validateUserToken user token =
  case userToken user of
   Just t
     | t == token -> Just True  -- ^ tokens match
     | otherwise  -> Just False -- ^ tokens don't match
   Nothing        -> Nothing    -- ^ there is no token (account already actived)

sendAccountActivationToken :: User -> LT.Text -> LT.Text -> IO ()
sendAccountActivationToken user body bodyHtml = do
     mail <- simpleMail
             (Address (Just $ userName user) (userEmail user))
             (Address (Just "LambdaCms") "lambdacms@example.com")
             "Account Activation"
             (body)
             (bodyHtml)
             []
     renderSendMail mail

getUserAdminOverviewR = do
    timeNow <- liftIO getCurrentTime
    hrtLocale <- lift lambdaCmsHumanTimeLocale
    (users :: [Entity User]) <- lift $ runDB $ selectList [] []
    lambdaCmsAdminLayoutSub $ do
      setTitleI MsgUserOverview
      $(whamletFile "templates/user/index.hamlet")

getUserAdminNewR = do
    eu <- liftIO emptyUser
    (formWidget, enctype) <- generateFormPost $ userForm eu (Just MsgCreate)
    lambdaCmsAdminLayoutSub $ do
      setTitleI MsgNewUser
      $(whamletFile "templates/user/new.hamlet")

postUserAdminNewR = do
    eu <- liftIO emptyUser
    ((formResult, formWidget), enctype) <- runFormPost $ userForm eu (Just MsgCreate)
    case formResult of
      FormSuccess user -> do

        case userToken user of
         Just token -> do
           userId <- lift $ runDB $ insert user
           html <- withUrlRenderer $(hamletFile "templates/mail/activation-html.hamlet")
           text <- withUrlRenderer $(hamletFile "templates/mail/activation-text.hamlet")
           let bodyHtml = renderHtml html
               bodyText = renderHtml text

           _ <- liftIO $ sendAccountActivationToken user bodyText bodyHtml
           setMessageI MsgSuccessCreate
           redirectUltDest $ UserAdminR userId
         Nothing -> error "No token found."
      _ -> do
        lambdaCmsAdminLayoutSub $ do
          setTitleI MsgNewUser
          $(whamletFile "templates/user/new.hamlet")

getUserAdminR userId = do
    user <- lift $ runDB $ get404 userId
    timeNow <- liftIO getCurrentTime
    hrtLocale <- lift lambdaCmsHumanTimeLocale
    (formWidget, enctype) <- generateFormPost $ userForm user (Just MsgSave)
    (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
    lambdaCmsAdminLayoutSub $ do
      setTitle . toHtml $ userName user
      $(whamletFile "templates/user/edit.hamlet")

postUserAdminR userId = do
  user <- lift . runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- lift lambdaCmsHumanTimeLocale
  ((formResult, formWidget), enctype) <- runFormPost $ userForm user (Just MsgSave)
  (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
  case formResult of
   FormSuccess updatedUser -> do
     _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
     setMessageI MsgSuccessReplace
     redirect $ UserAdminR userId
   _ -> do
    lambdaCmsAdminLayoutSub $ do
      setTitle . toHtml $ userName user
      $(whamletFile "templates/user/edit.hamlet")

postUserAdminChangePasswordR userId = do
  user <- lift . runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- lift lambdaCmsHumanTimeLocale
  (formWidget, enctype) <- generateFormPost $ userForm user (Just MsgSave)
  opw <- lookupPostParam "original-pw"
  ((formResult, pwFormWidget), pwEnctype) <- runFormPost $ userChangePasswordForm opw (Just MsgChange)
  case formResult of
   FormSuccess f -> do
     _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f)]
     setMessageI MsgSuccessChgPwd
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

getUserAdminActivateR userId token = do
  user <- lift . runDB $ get404 userId
  case validateUserToken user token of
   Just True -> do
     (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just MsgChange)
     defaultLayoutSub $ do
       setTitle . toHtml $ userName user
       $(whamletFile "templates/user/activate.hamlet")
   Just False -> defaultLayoutSub $ do
     setTitleI MsgTokenMismatch
     $(whamletFile "templates/user/tokenmismatch.hamlet")
   Nothing -> defaultLayoutSub $ do
     setTitleI MsgAccountAlreadyActivated
     $(whamletFile "templates/user/account-already-activated.hamlet")

postUserAdminActivateR userId token = do
  user <- lift . runDB $ get404 userId
  case validateUserToken user token of
   Just True -> do
     opw <- lookupPostParam "original-pw"
     ((formResult, pwFormWidget), pwEnctype) <- runFormPost $ userChangePasswordForm opw (Just MsgChange)
     case formResult of
      FormSuccess f -> do
        _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f), UserToken =. Nothing]
        setMessage "Msg: Successfully activated"
        redirect $ AdminHomeR
      _ -> do
        defaultLayoutSub $ do
          setTitle . toHtml $ userName user
          $(whamletFile "templates/user/activate.hamlet")
   Just False -> defaultLayoutSub $ do
     setTitleI MsgTokenMismatch
     $(whamletFile "templates/user/tokenmismatch.hamlet")
   Nothing -> defaultLayoutSub $ do
     setTitleI MsgAccountAlreadyActivated
     $(whamletFile "templates/user/account-already-activated.hamlet")
