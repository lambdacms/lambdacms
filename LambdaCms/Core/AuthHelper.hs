module LambdaCms.Core.AuthHelper
       ( generateUUID
       , generateActivationToken
       ) where

import           Data.Text    (Text)
import           Data.Text    (pack)
import           Data.UUID
import           Data.UUID.V4

-- | UUID as Text
generateUUID :: IO Text
generateUUID = nextRandom >>= return . pack . toString

-- | Same as generateUUID, for now.
generateActivationToken :: IO Text
generateActivationToken = generateUUID
