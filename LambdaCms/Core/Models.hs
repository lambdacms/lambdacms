{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module LambdaCms.Core.Models where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time.Clock
import Data.Typeable (Typeable)
import Prelude

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsCore"]
    $(persistFileWith lowerCaseSettings "config/models")
