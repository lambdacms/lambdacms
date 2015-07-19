{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module LambdaCms.Core.Handler.ActionLog
       ( getActionLogAdminIndexR
       , getActionLogAdminUserR
       ) where

import           Data.Int               (Int64)
import           Data.List              (intersect)
import           Data.Lists             (firstOr)
import           Data.String
import           Data.Text
import           Data.Time.Clock
import           Data.Time.Format.Human
import           Database.Esqueleto     ((^.))
import qualified Database.Esqueleto     as E
import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg
import           Network.Wai
import           Text.Read              (readEither)
import           Yesod.Core
import           Yesod.Core.Types


data JsonLog = JsonLog
               { message  :: Text
               , username :: Text
               , userUrl  :: Maybe Text
               , timeAgo  :: String
               }

-- TODO: The ToJSON instance could move directly to the ActionLog model.
instance ToJSON JsonLog where
    toJSON (JsonLog msg username' userUrl' timeAgo') =
        object [ "message"  .= msg
               , "username" .= username'
               , "userUrl"  .= userUrl'
               , "timeAgo"  .= timeAgo'
               ]

-- | REST/JSON endpoint for fetching the recent logs.
getActionLogAdminIndexR :: CoreHandler TypedContent
getActionLogAdminIndexR = getActionLogAdminJson Nothing

-- | REST/JSON endpoint for fetching the recent logs of a particular user.
getActionLogAdminUserR :: UserId -> CoreHandler TypedContent
getActionLogAdminUserR userId = getActionLogAdminJson (Just userId)


getActionLogAdminJson :: Maybe UserId -> CoreHandler TypedContent
getActionLogAdminJson mUserId = selectRep . provideRep $ do
    (limit, offset) <- getFilters
    lang            <- getCurrentLang
    can             <- lift getCan
    y               <- lift getYesod
    req             <- waiRequest
    timeNow         <- liftIO getCurrentTime
    hrtLocale       <- lift lambdaCmsHumanTimeLocale
    let renderUrl = flip (yesodRender y (resolveApproot y req)) []
        toAgo     = humanReadableTimeI18N' hrtLocale timeNow
    logs      <- getActionLogs mUserId limit offset lang
    jsonLogs  <- mapM (logToJsonLog can renderUrl toAgo) logs
    returnJson jsonLogs

getActionLogs :: Maybe UserId
              -> Int64
              -> Int64
              -> Text
              -> CoreHandler [(Entity ActionLog, Entity User)]
getActionLogs mUserId limit offset lang = do
    logs <- lift $ runDB
            $ E.select
            $ E.from $ \(log' `E.InnerJoin` user) -> do
                E.on $ log' ^. ActionLogUserId E.==. user ^. UserId
                E.where_ $ log' ^. ActionLogLang E.==. E.val lang
                maybe (return ()) (E.where_ . (E.==.) (user ^. UserId) . E.val) mUserId
                E.limit limit
                E.offset offset
                E.orderBy [E.desc (log' ^. ActionLogCreatedAt)]
                return (log', user)
    return logs

logToJsonLog :: (LambdaCmsAdmin master, IsString method, Monad m) =>
                (Route master -> method -> Maybe r)
                -> (r -> Text)
                -> (UTCTime -> String)
                -> (Entity ActionLog, Entity User)
                -> m JsonLog
logToJsonLog can renderUrl toAgo (Entity _ log', Entity userId user) = do
    let mUserUrl = renderUrl <$> (can (coreR $ UserAdminR $ UserAdminEditR userId) "GET")
    return $ JsonLog
             { message  = actionLogMessage log'
             , username = userName user
             , userUrl  = mUserUrl
             , timeAgo  = toAgo $ actionLogCreatedAt log'
             }

resolveApproot :: Yesod master => master -> Request -> ResolvedApproot
resolveApproot master req =
    case approot of
        ApprootRelative   -> ""
        ApprootStatic   t -> t
        ApprootMaster   f -> f master
        ApprootRequest  f -> f master req

getCurrentLang :: CoreHandler Text
getCurrentLang = do
    langs <- languages
    y <- lift getYesod
    return . firstOr "en" $ langs `intersect` (renderLanguages y)

getFilters :: CoreHandler (Int64, Int64)
getFilters = do
    mLimitText  <- lookupGetParam "limit"
    mOffsetText <- lookupGetParam "offset"
    case (defaultTo 10 mLimitText, defaultTo 0 mOffsetText) of
        (Left  _    , Left _      ) -> lift $ invalidArgsI [ Msg.InvalidLimit
                                                           , Msg.InvalidOffset ]
        (Left  _    , _           ) -> lift $ invalidArgsI [ Msg.InvalidLimit ]
        (_          , Left _      ) -> lift $ invalidArgsI [ Msg.InvalidOffset ]
        (Right limit, Right offset) -> return (limit, offset)
    where
        defaultTo d mText = maybe (Right d) id (readEither . unpack <$> mText)
