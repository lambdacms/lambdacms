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
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsMedia"] [persistLowerCase|
MediaFile
  location FilePath
  contentType Text
  label Text
  description Textarea Maybe
  uploadedAt UTCTime
  UniqueLocation location
  deriving Typeable Show
|]
