{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module LambdaCms.Core.Models where

import           Data.Text              (Text)
import           Data.Time.Clock
import           Data.Typeable          (Typeable)
import           Database.Persist.Quasi
import           Prelude
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsCore"]
    $(persistFileWith lowerCaseSettings "config/models")
