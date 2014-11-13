{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module LambdaCms.Media.Routes where

import Yesod
import LambdaCms.Media.Models

data Media = Media

mkYesodSubData "Media" [parseRoutes|
/                     MediaFileOverviewR  GET
/new                  MediaFileNewR       GET POST
/#MediaFileId/rename  MediaFileRenameR    POST
!/#MediaFileId        MediaFileR          GET POST DELETE
|]
