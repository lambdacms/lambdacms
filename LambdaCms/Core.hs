{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module LambdaCms.Core
    ( module Export
    , LambdaCmsAdmin
    , maybeAuth'
    , maybeAuthId'
    , authLoginDest
    , Core
    ) where

import           Database.Persist.Sql (SqlBackend)

import           LambdaCms.Core.Import
import           LambdaCms.Core.Models as Export
import           LambdaCms.Core.Routes as Export
import           LambdaCms.Core.Handler.Home as Export
import           LambdaCms.Core.Handler.User as Export



instance ( Yesod master
         , LambdaCmsAdmin master
         , SqlBackend ~ PersistMonadBackend (YesodPersistBackend master (HandlerT master IO))
         , PersistQuery (YesodPersistBackend master (HandlerT master IO))
         ) => YesodSubDispatch Core (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCore)
