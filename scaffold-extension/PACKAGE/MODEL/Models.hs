{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module %PACKAGE%.%MODEL%.Models where

import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Data.Typeable          (Typeable)
import           Database.Persist.Quasi
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrate%PACKAGE%%MODEL%"]
    $(persistFileWith lowerCaseSettings "config/models")
