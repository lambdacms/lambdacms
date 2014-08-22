{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LambdaCms.Core.Routes where

import Yesod
import           LambdaCms.Core.Models

data Core = Core

mkYesodSubData "Core" [parseRoutes|
/                     AdminHomeR        GET
/users                UserAdminR        GET
/users/new            UserAdminNewR     GET POST
/user/#UserId         UserAdminDetailR  GET
/user/#UserId/edit    UserAdminEditR    GET POST
/user/#UserId/delete  UserAdminDeleteR  POST
|]
