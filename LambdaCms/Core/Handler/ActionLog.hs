{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module LambdaCms.Core.Handler.ActionLog
       ( getActionLogAdminR
       ) where

import           Data.List              (intersect)
import           Data.Lists             (firstOr)
import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg
import           Yesod.Auth             (requireAuthId)
import qualified Database.Esqueleto     as E
import           Database.Esqueleto     ((^.))
import           Data.Time.Clock

getActionLogAdminR :: CoreHandler TypedContent
getActionLogAdminR = do
    langs <- languages
    y <- lift getYesod
    let lang = firstOr "en" $ langs `intersect` (renderLanguages y)
    authId <- lift requireAuthId

    selectRep $ do
        provideRep . lift $ do
            can <- getCan
            -- (logs :: [Entity ActionLog]) <- runDB $ selectList [ActionLogLang ==. lang] []
            logs <- runDB
                    $ E.select
                    $ E.from $ \(log `E.InnerJoin` user) -> do
                        E.on $ log ^. ActionLogUserId E.==. user ^. UserId
                        E.where_ $ log ^. ActionLogLang E.==. E.val lang
                        return (log, user)

            -- (personalLogs :: [Entity ActionLog]) <- runDB $ selectList [ ActionLogLang ==. lang
            --                                                            , ActionLogUserId ==. authId
            --                                                            ] []
            personalLogs <- runDB
                            $ E.select
                            $ E.from $ \(log `E.InnerJoin` user) -> do
                                E.on $ log ^. ActionLogUserId E.==. user ^. UserId
                                E.where_ $ log ^. ActionLogLang E.==. E.val lang
                                E.where_ $ user ^. UserId E.==. E.val authId
                                return (log, user)

            html <- adminLayout $(widgetFile "action-log")
            return html
        provideRep . lift $ do
            (logs :: [Entity ActionLog]) <- runDB $ selectList [ActionLogLang ==. lang] []
            returnJson logs
