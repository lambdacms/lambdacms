{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LambdaCms.Core.Handler.Home
  ( getAdminHomeR
  ) where

import           Data.Text              (pack)
import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg
import           Yesod.Auth             (requireAuthId)

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift $ do
    can <- getCan
    authId <- requireAuthId
    let feedCount = 11 :: Int

    adminLayout $ do
        setTitleI Msg.Dashboard
        $(widgetFile "adminhome")
