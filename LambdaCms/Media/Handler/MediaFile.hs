{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TemplateHaskell     #-}

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

getMediaFileOverviewR = lambdaCmsAdminLayoutSub $(whamletFile "templates/overview.hamlet")
getMediaFileNewR = lambdaCmsAdminLayoutSub $(whamletFile "templates/new.hamlet")
postMediaFileNewR = lambdaCmsAdminLayoutSub $(whamletFile "templates/new.hamlet")
getMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
postMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
deleteMediaFileR _ = lambdaCmsAdminLayoutSub $(whamletFile "templates/edit.hamlet")
