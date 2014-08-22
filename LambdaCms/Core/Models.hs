{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LambdaCms.Core.Models where

import Yesod
import Data.Text (Text)
--import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Prelude

share [mkPersist sqlOnlySettings, mkMigrate "migrateLambdaCmsCore"] [persistLowerCase|
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable Show
Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email
|]
