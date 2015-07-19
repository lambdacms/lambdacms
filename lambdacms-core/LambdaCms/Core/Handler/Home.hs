{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LambdaCms.Core.Handler.Home
  ( getAdminHomeR
  ) where

import           Data.Text              (pack)
import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg
import           Yesod.Auth             (requireAuthId)

-- | The home of the admin section, displaying a dashboard like page.
getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift $ do
    can <- getCan
    authId <- requireAuthId

    -- One less then the `actionLogChunkLength` will be displayed.
    -- The potential last item is used to determain if the "show more"
    -- link should be shown.
    let actionLogChunkLength = 11 :: Int

    adminLayout $ do
        setTitleI Msg.Dashboard
        $(widgetFile "adminhome")
