{-# LANGUAGE KindSignatures      #-}
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
  , chpassUserAdminEditR
  , rqpassUserAdminEditR
  , deactivateUserAdminEditR
  , activateUserAdminEditR
  , getUserAdminActivateR
  , postUserAdminActivateR
  ) where

import           LambdaCms.Core.Import
import           LambdaCms.Core.Message        (CoreMessage)
import qualified LambdaCms.Core.Message        as Msg
import           Yesod                         (Route)
import           Yesod.Auth                    (Creds (..), requireAuthId,
                                                setCreds)
import           Yesod.Auth.Email              (saltPass)

import           Control.Arrow                 ((&&&))
import           Data.Maybe                    (fromJust, fromMaybe, isJust)
import qualified Data.Set                      as S
import qualified Data.Text                     as T (breakOn, length, pack,
                                                     takeWhile)
import           Data.Time.Clock
import           Data.Time.Format.Human
import           Network.Mail.Mime
import           Text.Blaze.Html.Renderer.Text (renderHtml)


-- | Data type used by the change password form.
data ComparePassword = ComparePassword { originalPassword :: Text
                                       , _confirmPassword :: Text
                                       } deriving (Show, Eq)

-- | Form by which account setting are changed.
accountSettingsForm :: LambdaCmsAdmin master
                    => User
                    -> S.Set (Roles master)
                    -> Maybe CoreMessage
                    -> Html
                    -> MForm (HandlerT master IO)
                             (FormResult (User, [Roles master]), WidgetT master IO ())
accountSettingsForm user roles mlabel extra = do
    maRoles <- lift mayAssignRoles
    -- User fields
    (unameRes, unameView) <- mreq textField (bfs Msg.Username) (Just $ userName user)
    (emailRes, emailView) <- mreq emailField (bfs Msg.EmailAddress) (Just $ userEmail user)
    -- Roles field
    (rolesRes, mrolesView) <- if maRoles
        then do
            (rolesRes', rolesView) <- mreq (checkboxesField roleList)
                                           "Not used"
                                           (Just $ S.toList roles)
            return (rolesRes', Just rolesView)
        else return (FormSuccess $ S.toList roles, Nothing)

    let userRes = (\un ue -> user { userName = un, userEmail = ue })
                  <$> unameRes
                  <*> emailRes
        formRes = (,) <$> userRes <*> rolesRes
        widget = $(widgetFile "user/settings-form")

    return (formRes, widget)
  where
    roleList = optionsPairs $ map ((T.pack . show) &&& id) [minBound .. maxBound]

-- | Webform for changing a user's password.
userChangePasswordForm :: Maybe Text -> Maybe CoreMessage -> CoreForm ComparePassword
userChangePasswordForm original submit =
    renderBootstrap3 BootstrapBasicForm $ ComparePassword
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
    return User { userIdent     = uuid
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

-- | Validate an activation token.
validateUserToken :: User -> Text -> Maybe Bool
validateUserToken user token = case userToken user of
    Just t
      | t == token -> Just True  -- tokens match
      | otherwise  -> Just False -- tokens don't match
    Nothing        -> Nothing    -- there is no token (account already actived)

-- | Send an email to the user with a link containing the activation token.
sendAccountActivationToken :: Entity User -> CoreHandler ()
sendAccountActivationToken (Entity userId user) = case userToken user of
    Just token ->
        lift $ sendMailToUser user "Account activation"
            $(hamletFile "templates/mail/activation-text.hamlet")
            $(hamletFile "templates/mail/activation-html.hamlet")
    Nothing -> error "No activation token found"

-- | Send an email to the user with a link containing the reset token.
sendAccountResetToken :: Entity User -> CoreHandler ()
sendAccountResetToken (Entity userId user) = case userToken user of
    Just token ->
        lift $ sendMailToUser user "Account password reset"
            $(hamletFile "templates/mail/reset-text.hamlet")
            $(hamletFile "templates/mail/reset-html.hamlet")
    Nothing -> error "No reset token found"

-- | Function for sending mail to the user.  The method of sending mail is up
-- to the implementation by the `lambdaCmsSendMail` function in the "base"
--- application.
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

-- | Create a new user, show the form.
getUserAdminNewR :: CoreHandler Html
getUserAdminNewR = do
    eu <- liftIO emptyUser
    lift $ do
        can <- getCan
        drs <- defaultRoles
        (formWidget, enctype) <- generateFormPost $
            accountSettingsForm eu drs (Just Msg.Create)
        adminLayout $ do
            setTitleI Msg.NewUser
            $(widgetFile "user/new")

-- | Create a new user, handle a posted form.
postUserAdminNewR :: CoreHandler Html
postUserAdminNewR = do
    eu <- liftIO emptyUser
    drs <- lift defaultRoles
    ((formResult, formWidget), enctype) <- lift . runFormPost $
        accountSettingsForm eu drs (Just Msg.Create)
    case formResult of
        FormSuccess (user, roles) -> do
            userId <- lift $ runDB $ insert user
            lift $ setUserRoles userId (S.fromList roles)
            sendAccountActivationToken (Entity userId user)
            lift $ logUser user >>= logAction
            lift $ setMessageI Msg.SuccessCreate
            redirectUltDest $ UserAdminR UserAdminIndexR
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.NewUser
                $(widgetFile "user/new")

-- | Show the forms to edit an existing user.
getUserAdminEditR :: UserId -> CoreHandler Html
getUserAdminEditR userId = do
    timeNow <- liftIO getCurrentTime
    lift $ do
        authId <- requireAuthId
        can <- getCan
        user <- runDB $ get404 userId
        urs <- getUserRoles userId
        hrtLocale <- lambdaCmsHumanTimeLocale
        (formWidget, enctype) <- generateFormPost $
            accountSettingsForm user urs (Just Msg.Save)     -- user form
        (pwFormWidget, pwEnctype) <- generateFormPost $
            userChangePasswordForm Nothing (Just Msg.Change) -- user password form
        adminLayout $ do
            setTitleI . Msg.EditUser $ userName user
            $(widgetFile "user/edit")

-- | Change a user's main properties.
patchUserAdminEditR :: UserId -> CoreHandler Html
patchUserAdminEditR userId = do
    (user, timeNow, hrtLocale, urs) <- updateHelper userId
    (pwFormWidget, pwEnctype) <- lift . generateFormPost $
        userChangePasswordForm Nothing (Just Msg.Change)
    ((formResult, formWidget), enctype) <- lift . runFormPost $
        accountSettingsForm user urs (Just Msg.Save)
    case formResult of
        FormSuccess (updatedUser, updatedRoles) -> do
            _ <- lift $ runDB $ update userId [ UserName  =. userName  updatedUser
                                              , UserEmail =. userEmail updatedUser
                                              ]
            lift $ setUserRoles userId (S.fromList updatedRoles)
            lift $ logUser user >>= logAction
            lift $ setMessageI Msg.SuccessReplace
            redirect $ UserAdminR $ UserAdminEditR userId
        _ -> lift $ do
            authId <- requireAuthId
            can <- getCan
            adminLayout $ do
                setTitleI . Msg.EditUser $ userName user
                $(widgetFile "user/edit")

-- | Change a user's password.
--
-- Since 0.3.1.0
-- Store hashed passwords using `saltPass` from `Yesod.Auth.Email`.
chpassUserAdminEditR :: UserId -> CoreHandler Html
chpassUserAdminEditR userId = do
    authId <- lift requireAuthId
    if userId == authId
        then do
            (user, timeNow, hrtLocale, urs) <- updateHelper userId
            (formWidget, enctype) <- lift . generateFormPost $
                accountSettingsForm user urs (Just Msg.Save)
            opw <- lookupPostParam "original-pw"
            ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $
                userChangePasswordForm opw (Just Msg.Change)
            case formResult of
                FormSuccess f -> do
                    {- For now it's not clear what is the best way to store
                    passwords due to different plug-ins may store passwords
                    differently, but unmodified naked password are insecure
                    anyway.  The only one default plug-in from `Yesod.Auth`
                    which stores passwords internally is `Yesod.Auth.Email`,
                    and since it stores hashed passwords using `saltPass`
                    function it was decided to default to this approach for now.
                    -}
                    saltedPassword <- liftIO . saltPass $ originalPassword f
                    _ <- lift . runDB $
                        update userId [ UserPassword =. Just saltedPassword ]
                    lift $ logUser user >>= logAction
                    lift $ setMessageI Msg.SuccessChgPwd
                    redirect $ UserAdminR $ UserAdminEditR userId
                _ -> lift $ do
                    can <- getCan
                    adminLayout $ do
                        setTitleI . Msg.EditUser $ userName user
                        $(widgetFile "user/edit")
        else error "Can't change this uses password"

-- | Helper function to get data required for some DB updates operations in
-- handlers.  Removes code duplication.
updateHelper :: forall (t :: (* -> *) -> * -> *) site.
                ( MonadTrans t, MonadIO (t (HandlerT site IO))
                , LambdaCmsAdmin site )
             => Key User
             -> t (HandlerT site IO)
                  (User, UTCTime, HumanTimeLocale, S.Set (Roles site))
updateHelper userId = do
    user      <- lift . runDB $ get404 userId
    timeNow   <- liftIO getCurrentTime
    hrtLocale <- lift lambdaCmsHumanTimeLocale
    roles     <- lift $ getUserRoles userId
    return (user, timeNow, hrtLocale, roles)

-- | Request a user's password to be reset.
rqpassUserAdminEditR :: UserId -> CoreHandler Html
rqpassUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    token <- liftIO generateActivationToken
    let user = user'
               { userToken = Just token
               , userPassword = Nothing
               , userActive = False
               }
    _ <- lift . runDB $ replace userId user
    _ <- sendAccountResetToken (Entity userId user)
    lift $ logUser user >>= logAction
    lift $ setMessageI Msg.PasswordResetTokenSend
    redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Deactivate a user.
deactivateUserAdminEditR :: UserId -> CoreHandler Html
deactivateUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    case userToken user' of
        Nothing -> do
            let user = user' { userActive = False }

            _ <- lift . runDB $ replace userId user
            lift $ logUser user >>= logAction
            lift $ setMessageI Msg.UserDeactivated
        _ -> lift $ setMessageI Msg.UserStillPending
    redirectUltDest . UserAdminR $ UserAdminEditR userId

-- | Activate a user.
activateUserAdminEditR :: UserId -> CoreHandler Html
activateUserAdminEditR userId = do
    user' <- lift . runDB $ get404 userId
    case userToken user' of
        Nothing -> do
            let user = user' { userActive = True }

            _ <- lift . runDB $ replace userId user
            lift $ logUser user >>= logAction
            lift $ setMessageI Msg.UserActivated
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
                 { userEmail = random <> "@@@" <> userEmail user'
                 , userToken = Nothing
                 , userActive = False
                 , userDeletedAt = Just timeNow
                 }

        _ <- runDB $ replace userId user
        logAction =<< logUser user
        setMessageI Msg.SuccessDelete
    redirectUltDest $ UserAdminR UserAdminIndexR

-- | Active an account by emailed activation link.
getUserAdminActivateR :: UserId -> Text -> CoreHandler Html
getUserAdminActivateR userId token = do
    user <- lift . runDB $ get404 userId
    case validateUserToken user token of
        Just True -> do
            (pwFormWidget, pwEnctype) <- lift . generateFormPost $
                userChangePasswordForm Nothing (Just Msg.Save)
            lift . adminAuthLayout $ do
                setTitle . toHtml $ userName user
                $(widgetFile "user/activate")
        Just False -> lift . adminAuthLayout $ do
            setTitleI Msg.TokenMismatch
            $(widgetFile "user/tokenmismatch")
        Nothing -> lift . adminAuthLayout $ do
            setTitleI Msg.AccountAlreadyActivated
            $(widgetFile "user/account-already-activated")

-- | Process a password change by password-reset-link email.
postUserAdminActivateR :: UserId -> Text -> CoreHandler Html
postUserAdminActivateR userId token = do
    user <- lift . runDB $ get404 userId
    case validateUserToken user token of
        Just True -> do
            opw <- lookupPostParam "original-pw"
            ((formResult, pwFormWidget), pwEnctype) <- lift . runFormPost $
                userChangePasswordForm opw (Just Msg.Save)
            case formResult of
                FormSuccess f -> do
                    _ <- lift . runDB $
                        update userId [ UserPassword =. Just (originalPassword f)
                                      , UserToken    =. Nothing
                                      , UserActive   =. True
                                      ]
                    lift $ setMessageI Msg.ActivationSuccess
                    lift . setCreds False $
                        Creds "lambdacms-token-activation" (userEmail user) []
                    redirect AdminHomeR
                _ -> lift . adminAuthLayout $ do
                        setTitle . toHtml $ userName user
                        $(widgetFile "user/activate")
        Just False -> lift . adminAuthLayout $ do
            setTitleI Msg.TokenMismatch
            $(widgetFile "user/tokenmismatch")
        Nothing -> lift . adminAuthLayout $ do
            setTitleI Msg.AccountAlreadyActivated
            $(widgetFile "user/account-already-activated")
