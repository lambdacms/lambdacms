{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}

module LambdaCms.Media.Handler.Home
       ( getMediaHomeR
       ) where

import LambdaCms.Media.Import

getMediaHomeR :: MediaHandler Html
getMediaHomeR = withUrlRenderer [hamlet|not yet implemented|]
