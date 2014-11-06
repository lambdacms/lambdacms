{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module LambdaCms.Media.Models where

import Yesod
import Data.Text (Text)
import Data.Typeable (Typeable)

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsMedia"] [persistLowerCase|
MediaFile
  name Text
  location Text
  deriving Typeable Show
|]
