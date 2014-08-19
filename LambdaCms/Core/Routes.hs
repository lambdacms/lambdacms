{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module LambdaCms.Core.Routes where

import Yesod

data Core = Core

mkYesodSubData "Core" [parseRoutes|
/        AdminHomeR   GET
/users   AdminUsersR  GET
|]
