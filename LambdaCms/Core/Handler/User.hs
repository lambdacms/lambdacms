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
  ( getUserAdminIndexR
  , getUserAdminNewR
  , postUserAdminNewR
  , getUserAdminEditR
  , postUserAdminEditR
  , deleteUserAdminEditR
  , postUserAdminChangePasswordR
  , getUserAdminActivateR
  , postUserAdminActivateR
  ) where

import LambdaCms.Core.Import
import LambdaCms.Core.AuthHelper
import LambdaCms.Core.Message (CoreMessage)
import qualified LambdaCms.Core.Message as Msg
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

getUserAdminIndexR           :: CoreHandler Html
getUserAdminNewR             :: CoreHandler Html
postUserAdminNewR            :: CoreHandler Html
getUserAdminEditR            :: UserId -> CoreHandler Html
postUserAdminEditR           :: UserId -> CoreHandler Html
postUserAdminChangePasswordR :: UserId -> CoreHandler Html
deleteUserAdminEditR         :: UserId -> CoreHandler Html
getUserAdminActivateR        :: UserId -> Text -> CoreHandler Html
postUserAdminActivateR       :: UserId -> Text -> CoreHandler Html

userForm :: User -> Maybe CoreMessage -> CoreForm User
userForm u submit = renderBootstrap3 BootstrapBasicForm $ User
             <$> pure            (userIdent u)
             <*> areq textField  (bfs Msg.Username)        (Just $ userName u)
             <*> pure            (userPassword u)
             <*> areq emailField (bfs Msg.EmailAddress)   (Just $ userEmail u)
             <*> pure            (userToken u)
             <*> pure            (userCreatedAt u)
             <*> pure            (userLastLogin u)
             <*  bootstrapSubmit (BootstrapSubmit (fromMaybe Msg.Submit submit) " btn-success " [])

userChangePasswordForm :: Maybe Text -> Maybe CoreMessage -> CoreForm ComparePassword
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

sendAccountActivationToken :: (LambdaCmsAdmin a) => a -> User -> LT.Text -> LT.Text -> IO ()
sendAccountActivationToken core user body bodyHtml = do
     mail <- simpleMail
             (Address (Just $ userName user) (userEmail user))
             (Address (Just "LambdaCms") "lambdacms@example.com")
             "Account Activation"
             (body)
             (bodyHtml)
             []
     lambdaCmsSendMail core mail

getUserAdminIndexR = do
  timeNow <- liftIO getCurrentTime
  lift $ do
    hrtLocale <- lambdaCmsHumanTimeLocale
    (users :: [Entity User]) <- runDB $ selectList [] []
    adminLayout $ do
      setTitleI Msg.UserIndex
      $(widgetFile "user/index")

getUserAdminNewR = do
  eu <- liftIO emptyUser
  lift $ do
    (formWidget, enctype) <- generateFormPost $ userForm eu (Just Msg.Create)
    adminLayout $ do
      setTitleI Msg.NewUser
      $(widgetFile "user/new")

postUserAdminNewR = do
    eu <- liftIO emptyUser
    ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm eu (Just Msg.Create)
    case formResult of
      FormSuccess user -> do

        case userToken user of
         Just token -> do
           userId <- lift $ runDB $ insert user
           html <- lift $ withUrlRenderer $(hamletFile "templates/mail/activation-html.hamlet")
           text <- lift $ withUrlRenderer $(hamletFile "templates/mail/activation-text.hamlet")
           y <- lift getYesod
           let bodyHtml = renderHtml html
               bodyText = renderHtml text

           _ <- liftIO $ sendAccountActivationToken y user bodyText bodyHtml
           lift $ setMessageI Msg.SuccessCreate
           redirectUltDest $ UserAdminR $ UserAdminEditR userId
         Nothing -> error "No token found."
      _ -> do
        lift . adminLayout $ do
          setTitleI Msg.NewUser
          $(widgetFile "user/new")

getUserAdminEditR userId = do
    timeNow <- liftIO getCurrentTime
    lift $ do
      user <- runDB $ get404 userId
      hrtLocale <- lambdaCmsHumanTimeLocale
      (formWidget, enctype) <- generateFormPost $ userForm user (Just Msg.Save)
      (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
      adminLayout $ do
        setTitleI . Msg.EditUser $ userName user
        $(widgetFile "user/edit")

postUserAdminEditR userId = do
  user <- lift . runDB $ get404 userId
  timeNow <- liftIO getCurrentTime
  hrtLocale <- lift lambdaCmsHumanTimeLocale
  ((formResult, formWidget), enctype) <- lift . runFormPost $ userForm user (Just Msg.Save)
  (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
  case formResult of
   FormSuccess updatedUser -> do
     _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
     lift $ setMessageI Msg.SuccessReplace
     redirect $ UserAdminR $ UserAdminEditR userId
   _ -> do
     lift . adminLayout $ do
       setTitleI . Msg.EditUser $ userName user
       $(widgetFile "user/edit")

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
     redirect $ UserAdminR $ UserAdminEditR userId
   _ -> do
     lift . adminLayout $ do
       setTitleI . Msg.EditUser $ userName user
       $(widgetFile "user/edit")

deleteUserAdminEditR userId = do
  lift $ do
    user <- runDB $ get404 userId
    _ <- runDB $ delete userId
    setMessageI Msg.SuccessDelete
  redirectUltDest $ UserAdminR UserAdminIndexR

getUserAdminActivateR userId token = do
  user <- lift . runDB $ get404 userId
  case validateUserToken user token of
   Just True -> do
     (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
     lift . adminLayout $ do
       setTitle . toHtml $ userName user
       $(widgetFile "user/activate")
   Just False -> lift . adminLayout $ do
     setTitleI Msg.TokenMismatch
     $(widgetFile "user/tokenmismatch")
   Nothing -> lift . adminLayout $ do
     setTitleI Msg.AccountAlreadyActivated
     $(widgetFile "user/account-already-activated")

postUserAdminActivateR userId token = do
  user <- lift . runDB $ get404 userId
  case validateUserToken user token of
   Just True -> do
     opw <- lookupPostParam "original-pw"
     ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $ userChangePasswordForm opw (Just Msg.Change)
     case formResult of
      FormSuccess f -> do
        _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f), UserToken =. Nothing]
        setMessage "Msg: Successfully activated"
        redirect $ AdminHomeR
      _ -> do
        lift . adminLayout $ do
          setTitle . toHtml $ userName user
          $(widgetFile "user/activate")
   Just False -> lift . adminLayout $ do
     setTitleI Msg.TokenMismatch
     $(widgetFile "user/tokenmismatch")
   Nothing -> lift . adminLayout $ do
     setTitleI Msg.AccountAlreadyActivated
     $(widgetFile "user/account-already-activated")
