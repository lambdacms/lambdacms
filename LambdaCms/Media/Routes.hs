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
/                     MediaHomeR      GET
/media                MediaFileOverviewR  GET
/media/new            MediaFileNewR       GET POST
!/media/#MediaFileId  MediaFileR          GET POST DELETE
|]
