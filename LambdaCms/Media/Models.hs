{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module LambdaCms.Media.Models where

import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Data.Typeable          (Typeable)
import           Database.Persist.Quasi
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsMedia"]
    $(persistFileWith lowerCaseSettings "config/models")
