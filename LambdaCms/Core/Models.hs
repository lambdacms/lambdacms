{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module LambdaCms.Core.Models where

import           Data.Text       (Text)
import           Yesod
--import Database.Persist.Quasi
import           Data.Time.Clock
import           Data.Typeable   (Typeable)
import           Prelude

share [mkPersist sqlSettings, mkMigrate "migrateLambdaCmsCore"] [persistLowerCase|
User
    ident Text
    name Text
    password Text Maybe
    email Text
    token Text Maybe
    createdAt UTCTime
    lastLogin UTCTime
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
