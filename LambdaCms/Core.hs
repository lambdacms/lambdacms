{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}

module LambdaCms.Core
    ( module Export
    , LambdaCmsAdmin
    , maybeAuth'
    , maybeAuthId'
    , authLoginDest
    , Core
    ) where

import           LambdaCms.Core.Import
import           LambdaCms.Core.Models as Export
import           LambdaCms.Core.Routes as Export
import           LambdaCms.Core.Handler.Home as Export
import           LambdaCms.Core.Handler.User as Export



instance (Yesod master, LambdaCmsAdmin master) => YesodSubDispatch Core (HandlerT master IO) where
--instance LambdaCmsAdmin master => YesodSubDispatch Core (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCore)
