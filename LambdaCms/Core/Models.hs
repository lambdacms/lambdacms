{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module LambdaCms.Core.Models where

import Yesod
--import Data.Text (Text)
--import Database.Persist.Quasi
--import Data.Typeable (Typeable)
import Prelude

-- Just some test models for now, this should also contian the User/Email thingy...
share [mkPersist sqlOnlySettings, mkMigrate "migrateLambdaCmsCore"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]
