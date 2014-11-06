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
/media                MediaOverviewR  GET
/media/new            MediaNewR       GET POST
!/media/#MediaFileId  MediaR          GET POST DELETE
|]
