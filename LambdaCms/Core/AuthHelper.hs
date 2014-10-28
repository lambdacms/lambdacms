module LambdaCms.Core.AuthHelper
       ( generateUUID
       ) where

import           Data.Text (Text)
import           Data.Text (pack)
import           Data.UUID.V4
import           Data.UUID

-- | UUID as Text
generateUUID :: IO Text
generateUUID = nextRandom >>= return . pack . toString
