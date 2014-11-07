{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}

module LambdaCms.Core
    ( module Export
    , LambdaCmsAdmin (..)
    , tryoutLayout
    , lambdaCoreLayout
    -- , maybeAuth'
    -- , maybeAuthId'
    -- , authLoginDest
    -- , extensions
    , LambdaCmsExtension (..)
    ) where

import           Database.Persist.Sql (SqlBackend)

import           LambdaCms.Core.Import
import           LambdaCms.Core.Models        as Export
import           LambdaCms.Core.Routes        as Export
import           LambdaCms.Core.Handler.Home  as Export
import           LambdaCms.Core.Handler.User  as Export
import           LambdaCms.Core.AuthHelper    as Export

-- instance ( Yesod master
--          , LambdaCmsAdmin master
--          , SqlBackend ~ (YesodPersistBackend master)
--          , PersistQuery (YesodPersistBackend master)
--          ) => YesodSubDispatch Core (HandlerT master IO) where
instance LambdaCmsAdmin master => YesodSubDispatch Core (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCore)
