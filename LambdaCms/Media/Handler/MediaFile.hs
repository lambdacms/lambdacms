{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}

module LambdaCms.Media.Handler.MediaFile
       ( getMediaOverviewR
       , getMediaNewR
       , postMediaNewR
       , getMediaR
       , postMediaR
       , deleteMediaR
       ) where

import LambdaCms.Media.Import

getMediaOverviewR :: MediaHandler Html
getMediaNewR      :: MediaHandler Html
postMediaNewR     :: MediaHandler Html
getMediaR         :: MediaFileId -> MediaHandler Html
postMediaR        :: MediaFileId -> MediaHandler Html
deleteMediaR      :: MediaFileId -> MediaHandler Html

getMediaOverviewR = withUrlRenderer [hamlet|not yet implmented|]
getMediaNewR = withUrlRenderer [hamlet|not yet implmented|]
postMediaNewR = withUrlRenderer [hamlet|not yet implmented|]
getMediaR _ = withUrlRenderer [hamlet|not yet implmented|]
postMediaR _ = withUrlRenderer [hamlet|not yet implmented|]
deleteMediaR _ = withUrlRenderer [hamlet|not yet implmented|]
