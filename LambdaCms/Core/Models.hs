{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module LambdaCms.Core.Models where

import Yesod
import Data.Text (Text)
--import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Prelude

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsCore"] [persistLowerCase|
User
    ident Text
    name Text
    password Text Maybe
    email Text
    token Text Maybe
    UniqueUser ident
    UniqueName name
    UniqueEmail email
    deriving Typeable Show
-- Email
--     email Text
--     user UserId Maybe
--     verkey Text Maybe
--     UniqueEmail email
|]
