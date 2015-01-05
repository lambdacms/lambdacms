{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LambdaCms.Core.Handler.Home
  ( getAdminHomeR
  ) where

import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg
import           Yesod.Auth             (requireAuthId)

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift $ do
    can <- getCan
    authId <- requireAuthId
    adminLayout $ do
        setTitleI Msg.Dashboard
        $(widgetFile "adminhome")
