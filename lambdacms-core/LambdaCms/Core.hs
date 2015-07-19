{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

{-|
Module      : Core
Description : Exports all relevant modules for the lambdacms-core subsite.
Copyright   : (c) Hoppinger BV, 2014-2015

This is the module to import when adding LambdaCms "core" functionality
to a Yesod application or subsite.
-}

module LambdaCms.Core
    ( module Export
    ) where

import           Database.Persist.Sql             ()

import           LambdaCms.Core.Classes           as Export
import           LambdaCms.Core.Foundation        as Export
import           LambdaCms.Core.Handler.ActionLog as Export
import           LambdaCms.Core.Handler.Home      as Export
import           LambdaCms.Core.Handler.Static    as Export
import           LambdaCms.Core.Handler.User      as Export
import           LambdaCms.Core.Import
import           LambdaCms.Core.Models            as Export
import           Network.Mail.Mime                as Export
-- instance ( Yesod master
--          , LambdaCmsAdmin master
--          , SqlBackend ~ (YesodPersistBackend master)
--          , PersistQuery (YesodPersistBackend master)
--          ) => YesodSubDispatch Core (HandlerT master IO) where
instance LambdaCmsAdmin master => YesodSubDispatch CoreAdmin (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCoreAdmin)
