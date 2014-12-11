{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}


module LambdaCms.Core.Handler.User
  ( getUserAdminIndexR
  , getUserAdminNewR
  , postUserAdminNewR
  , getUserAdminEditR
  , patchUserAdminEditR
  , deleteUserAdminEditR
  , chngpwdUserAdminEditR
  , rqstpwdUserAdminEditR
  , disableUserAdminEditR
  , enableUserAdminEditR
  , getUserAdminActivateR
  , postUserAdminActivateR
  ) where

import           LambdaCms.Core.Import
import           LambdaCms.Core.Message        (CoreMessage)
import qualified LambdaCms.Core.Message        as Msg
import           LambdaCms.I18n
import           Yesod                         (Route)
import           Yesod.Auth                    (requireAuthId)

import qualified Data.Text                     as T (breakOn, concat, length,
                                                     pack, takeWhile)
import qualified Data.Text.Lazy                as LT (Text)
import           Data.Time.Format

import           Control.Arrow                 ((&&&))
import           Data.Maybe                    (fromJust, fromMaybe, isJust)
import qualified Data.Set                      as S
import           Data.Time.Clock
import           Data.Time.Format.Human
import           Network.Mail.Mime
import           System.Locale
import           Text.Blaze.Html.Renderer.Text (renderHtml)

-- data type for a form to change a user's password
data ComparePassword = ComparePassword { originalPassword :: Text
                                       , confirmPassword  :: Text
                                       } deriving (Show, Eq)

accountSettingsForm :: LambdaCmsAdmin master
                    => User
                    -> S.Set (Roles master)
                    -> Maybe CoreMessage
                    -> Html
                    -> MForm (HandlerT master IO) (FormResult (User, [Roles master]), WidgetT master IO ())
accountSettingsForm user roles mlabel extra = do
    -- User fields
    (unameRes, unameView) <- mreq textField (bfs Msg.Username) (Just $ userName user)
    (emailRes, emailView) <- mreq emailField (bfs Msg.EmailAddress) (Just $ userEmail user)
    -- Roles field
    (rolesRes, rolesView) <- mreq (checkboxesField roleList) "Not used" (Just $ S.toList roles)
    let userRes = (\un ue -> user { userName = un, userEmail = ue })
                  <$> unameRes
                  <*> emailRes
        formRes = (,) <$> userRes <*> rolesRes
        widget = $(widgetFile "user/settings-form")
    return (formRes, widget)
    where roleList = optionsPairs $ map ((T.pack . show) &&& id) [minBound .. maxBound]

-- | Webform for changing a user's password.
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

-- | Helper to create a user with email address.
generateUserWithEmail :: Text -> IO User
generateUserWithEmail e = do
    uuid <- generateUUID
    token <- generateActivationToken
    timeNow <- getCurrentTime
    return $ User { userIdent     = uuid
                  , userName      = fst $ T.breakOn "@" e
                  , userPassword  = Nothing
                  , userEmail     = e
                  , userActive    = False
                  , userToken     = Just token
                  , userCreatedAt = timeNow
                  , userLastLogin = Nothing
                  , userDeletedAt = Nothing
                  }

-- | Helper to create an empty user.
emptyUser :: IO User
emptyUser = generateUserWithEmail ""

-- | Validate an activation token
validateUserToken :: User -> Text -> Maybe Bool
validateUserToken user token =
    case userToken user of
        Just t
          | t == token -> Just True  -- tokens match
          | otherwise  -> Just False -- tokens don't match
        Nothing        -> Nothing    -- there is no token (account already actived)

sendAccountActivationToken :: Entity User -> CoreHandler ()
sendAccountActivationToken (Entity userId user) = case userToken user of
    Just token -> do
        lift $ sendMailToUser user "Account activation"
            $(hamletFile "templates/mail/activation-text.hamlet")
            $(hamletFile "templates/mail/activation-html.hamlet")
    Nothing -> error "No activation token found"

sendAccountResetToken :: Entity User -> CoreHandler ()
sendAccountResetToken (Entity userId user) = case userToken user of
    Just token -> do
        lift $ sendMailToUser user "Account password reset"
            $(hamletFile "templates/mail/reset-text.hamlet")
            $(hamletFile "templates/mail/reset-html.hamlet")
    Nothing -> error "No reset token found"

sendMailToUser :: LambdaCmsAdmin master
               => User
               -> Text
               -> ((Route master -> [(Text, Text)] -> Text) -> Html)
               -> ((Route master -> [(Text, Text)] -> Text) -> Html)
               -> HandlerT master IO ()
sendMailToUser user subj ttemp htemp = do
    text <- getRenderedTemplate ttemp
    html <- getRenderedTemplate htemp
    mail <- liftIO $ simpleMail
            (Address (Just $ userName user) (userEmail user))
            (Address (Just "LambdaCms") "lambdacms@example.com")
            subj
            text
            html
            []

    lambdaCmsSendMail mail
    where
        getRenderedTemplate template = do
            markup <- withUrlRenderer template
            return $ renderHtml markup


-- | User overview.
getUserAdminIndexR :: CoreHandler Html
getUserAdminIndexR = do
    timeNow <- liftIO getCurrentTime
    lift $ do
      can <- getCan
      (users' :: [Entity User]) <- runDB $ selectList [UserDeletedAt ==. Nothing] []
      users <- mapM (\user -> do
                       ur <- getUserRoles $ entityKey user
                       return (user, S.toList ur)
                    ) users'
      hrtLocale <- lambdaCmsHumanTimeLocale
      adminLayout $ do
          setTitleI Msg.UserIndex
          $(widgetFile "user/index")

-- | Create a new user.
getUserAdminNewR :: CoreHandler Html
getUserAdminNewR = do
    eu <- liftIO emptyUser
    lift $ do
        can <- getCan
        (formWidget, enctype) <- generateFormPost $ accountSettingsForm eu S.empty (Just Msg.Create)
        adminLayout $ do
            setTitleI Msg.NewUser
            $(widgetFile "user/new")

-- | Create a new user.
postUserAdminNewR :: CoreHandler Html
postUserAdminNewR = do
    eu <- liftIO emptyUser
    ((formResult, formWidget), enctype) <- lift . runFormPost $ accountSettingsForm eu S.empty (Just Msg.Create)
    case formResult of
        FormSuccess (user, roles) -> do
            userId <- lift $ runDB $ insert user
            lift $ setUserRoles userId (S.fromList roles)
            _ <- sendAccountActivationToken (Entity userId user)
            lift $ setMessageI Msg.SuccessCreate
            redirectUltDest $ UserAdminR UserAdminIndexR
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.NewUser
                $(widgetFile "user/new")

-- | Edit an existing user.
getUserAdminEditR :: UserId -> CoreHandler Html
getUserAdminEditR userId = do
    timeNow <- liftIO getCurrentTime
    lift $ do
        authId <- requireAuthId
        can <- getCan
        user <- runDB $ get404 userId
        urs <- getUserRoles userId
        hrtLocale <- lambdaCmsHumanTimeLocale
        (formWidget, enctype)     <- generateFormPost $ accountSettingsForm user urs (Just Msg.Save)     -- user form
        (pwFormWidget, pwEnctype) <- generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change) -- user password form
        adminLayout $ do
            setTitleI . Msg.EditUser $ userName user
            $(widgetFile "user/edit")

-- | Edit an existing user.
patchUserAdminEditR :: UserId -> CoreHandler Html
patchUserAdminEditR userId = do
    user <- lift . runDB $ get404 userId
    timeNow <- liftIO getCurrentTime
    hrtLocale <- lift lambdaCmsHumanTimeLocale
    urs <- lift $ getUserRoles userId
    (pwFormWidget, pwEnctype)           <- lift . generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
    ((formResult, formWidget), enctype) <- lift . runFormPost $ accountSettingsForm user urs (Just Msg.Save)
    case formResult of
        FormSuccess (updatedUser, updatedRoles) -> do
            _ <- lift $ runDB $ update userId [UserName =. userName updatedUser, UserEmail =. userEmail updatedUser]
            lift $ setUserRoles userId (S.fromList updatedRoles)
            lift $ setMessageI Msg.SuccessReplace
            redirect $ UserAdminR $ UserAdminEditR userId
        _ -> lift $ do
            authId <- requireAuthId
            can <- getCan
            adminLayout $ do
                setTitleI . Msg.EditUser $ userName user
                $(widgetFile "user/edit")

-- | Edit password of an existing user.
chngpwdUserAdminEditR :: UserId -> CoreHandler Html
chngpwdUserAdminEditR userId = do
    authId <- lift requireAuthId
    case userId == authId of
        True -> do
            user <- lift . runDB $ get404 userId
            timeNow <- liftIO getCurrentTime
            hrtLocale <- lift lambdaCmsHumanTimeLocale
            urs <- lift $ getUserRoles userId
            (formWidget, enctype) <- lift . generateFormPost $ accountSettingsForm user urs (Just Msg.Save)
            opw <- lookupPostParam "original-pw"
            ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $ userChangePasswordForm opw (Just Msg.Change)
            case formResult of
                FormSuccess f -> do
                    _ <- lift . runDB $ update userId [UserPassword =. Just (originalPassword f)]
                    lift $ setMessageI Msg.SuccessChgPwd
                    redirect $ UserAdminR $ UserAdminEditR userId
                _ -> lift $ do
                    can <- getCan
                    adminLayout $ do
                        setTitleI . Msg.EditUser $ userName user
                        $(widgetFile "user/edit")
        False -> error "Can't change this uses password"

rqstpwdUserAdminEditR :: UserId -> CoreHandler Html
rqstpwdUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    token <- liftIO generateActivationToken
    let user = user'
               { userToken = Just token
               , userPassword = Nothing
               , userActive = False
               }
    _ <- lift . runDB $ replace userId user
    _ <- sendAccountResetToken (Entity userId user)
    lift $ setMessageI Msg.PasswordResetTokenSend
    redirectUltDest . UserAdminR $ UserAdminEditR userId

disableUserAdminEditR :: UserId -> CoreHandler Html
disableUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    case userToken user' of
        Nothing -> do
            let user = user' { userActive = False }

            _ <- lift . runDB $ replace userId user
            lift $ setMessageI Msg.UserDisabled
        _ -> lift $ setMessageI Msg.UserStillPending
    redirectUltDest . UserAdminR $ UserAdminEditR userId

enableUserAdminEditR :: UserId -> CoreHandler Html
enableUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    case userToken user' of
        Nothing -> do
            let user = user' { userActive = True }

            _ <- lift . runDB $ replace userId user
            lift $ setMessageI Msg.UserEnabled
        _ -> lift $ setMessageI Msg.UserStillPending
    redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Delete an existing user.
-- TODO: Don\'t /actually/ delete the DB record!
deleteUserAdminEditR :: UserId -> CoreHandler Html
deleteUserAdminEditR userId = do
    lift $ do
        user' <- runDB $ get404 userId
        timeNow <- liftIO getCurrentTime
        uuid <- liftIO generateUUID
        let random = T.takeWhile (/= '-') uuid
        let user = user'
                 { userEmail = random <> "@@@" <> (userEmail user')
                 , userToken = Nothing
                 , userActive = False
                 , userDeletedAt = Just timeNow
                 }

        _ <- runDB $ replace userId user
        setMessageI Msg.SuccessDelete
    redirectUltDest $ UserAdminR UserAdminIndexR

-- | Active an account.
getUserAdminActivateR :: UserId -> Text -> CoreHandler Html
getUserAdminActivateR userId token = do
    user <- lift . runDB $ get404 userId
    case validateUserToken user token of
        Just True -> do
            (pwFormWidget, pwEnctype) <- lift . generateFormPost $ userChangePasswordForm Nothing (Just Msg.Change)
            lift . defaultLayout $ do
                setTitle . toHtml $ userName user
                $(widgetFile "user/activate")
        Just False -> lift . defaultLayout $ do
            setTitleI Msg.TokenMismatch
            $(widgetFile "user/tokenmismatch")
        Nothing -> lift . defaultLayout $ do
            setTitleI Msg.AccountAlreadyActivated
            $(widgetFile "user/account-already-activated")

-- | Active an account.
postUserAdminActivateR :: UserId -> Text -> CoreHandler Html
postUserAdminActivateR userId token = do
    user <- lift . runDB $ get404 userId
    case validateUserToken user token of
        Just True -> do
            opw <- lookupPostParam "original-pw"
            ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $ userChangePasswordForm opw (Just Msg.Change)
            case formResult of
                FormSuccess f -> do
                    _ <- lift . runDB $ update userId [ UserPassword =. Just (originalPassword f)
                                                      , UserToken =. Nothing
                                                      , UserActive =. True
                                                      ]
                    setMessage "Msg: Successfully activated"
                    redirect $ AdminHomeR
                _ -> do
                    lift . defaultLayout $ do
                        setTitle . toHtml $ userName user
                        $(widgetFile "user/activate")
        Just False -> lift . defaultLayout $ do
            setTitleI Msg.TokenMismatch
            $(widgetFile "user/tokenmismatch")
        Nothing -> lift . defaultLayout $ do
            setTitleI Msg.AccountAlreadyActivated
            $(widgetFile "user/account-already-activated")
