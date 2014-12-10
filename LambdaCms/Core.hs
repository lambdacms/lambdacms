{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}

module LambdaCms.Core
    ( module Export
    ) where

import           Database.Persist.Sql (SqlBackend)

import           LambdaCms.Core.Import
import           LambdaCms.Core.Foundation     as Export
import           LambdaCms.Core.Models         as Export
import           LambdaCms.Core.Classes        as Export
import           LambdaCms.Core.Handler.Static as Export
import           LambdaCms.Core.Handler.Home   as Export
import           LambdaCms.Core.Handler.User   as Export
import           Network.Mail.Mime             as Export
-- instance ( Yesod master
--          , LambdaCmsAdmin master
--          , SqlBackend ~ (YesodPersistBackend master)
--          , PersistQuery (YesodPersistBackend master)
--          ) => YesodSubDispatch Core (HandlerT master IO) where
instance LambdaCmsAdmin master => YesodSubDispatch CoreAdmin (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCoreAdmin)
