{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns       #-}

module LambdaCms.Core.Routes where

import Yesod
import LambdaCms.Core.Models
import Data.Text

data Core = Core

mkYesodSubData "Core" [parseRoutes|
/                       AdminHomeR                 GET
/users                  UserAdminOverviewR         GET
/user/new               UserAdminNewR              GET POST
!/user/#UserId          UserAdminR                 GET POST DELETE
/user/#UserId/reset     UserAdminChangePasswordR   POST
/activate/#UserId/#Text UserAdminActivateR         GET POST
|]
