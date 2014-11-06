{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}

module LambdaCms.Media.Handler.MediaFile
       ( getMediaFileOverviewR
       , getMediaFileNewR
       , postMediaFileNewR
       , getMediaFileR
       , postMediaFileR
       , deleteMediaFileR
       ) where

import LambdaCms.Media.Import

getMediaFileOverviewR :: MediaHandler Html
getMediaFileNewR      :: MediaHandler Html
postMediaFileNewR     :: MediaHandler Html
getMediaFileR         :: MediaFileId -> MediaHandler Html
postMediaFileR        :: MediaFileId -> MediaHandler Html
deleteMediaFileR      :: MediaFileId -> MediaHandler Html

getMediaFileOverviewR = withUrlRenderer [hamlet|not yet implmented|]
getMediaFileNewR = withUrlRenderer [hamlet|not yet implmented|]
postMediaFileNewR = withUrlRenderer [hamlet|not yet implmented|]
getMediaFileR _ = withUrlRenderer [hamlet|not yet implmented|]
postMediaFileR _ = withUrlRenderer [hamlet|not yet implmented|]
deleteMediaFileR _ = withUrlRenderer [hamlet|not yet implmented|]
