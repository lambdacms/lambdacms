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

getMediaFileOverviewR = tryoutLayout [whamlet|not yet implmented|]
getMediaFileNewR = tryoutLayout [whamlet|not yet implmented|]
postMediaFileNewR = tryoutLayout [whamlet|not yet implmented|]
getMediaFileR _ = tryoutLayout [whamlet|not yet implmented|]
postMediaFileR _ = tryoutLayout [whamlet|not yet implmented|]
deleteMediaFileR _ = tryoutLayout [whamlet|not yet implmented|]
