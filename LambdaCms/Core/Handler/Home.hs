{-# LANGUAGE TemplateHaskell     #-}

module LambdaCms.Core.Handler.Home
  ( getAdminHomeR
  ) where

import           LambdaCms.Core.Import
import qualified LambdaCms.Core.Message as Msg

getAdminHomeR :: CoreHandler Html
getAdminHomeR = lift . adminLayout $ do
    setTitleI Msg.Dashboard
    $(widgetFile "adminhome")
