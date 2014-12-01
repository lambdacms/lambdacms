{-# LANGUAGE CPP #-}
module LambdaCms.Core.Import
    ( module Import
    ) where

import           Prelude                    as Import hiding (head, init, last, readFile, tail, writeFile)
import           Yesod                      as Import hiding (Route (..))
import           Yesod.Form.Bootstrap3      as Import

import           Control.Applicative        as Import (pure, (<$>), (<*>), (<*))
import           Data.Text                  as Import (Text)
import           Text.Hamlet                as Import (hamletFile)

--import           Foundation           as Import
import           LambdaCms.Core.Foundation  as Import
import           LambdaCms.Core.Models      as Import
import           LambdaCms.Core.Settings    as Import
--import           Settings             as Import
--import           Settings.Development as Import
--import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import           Data.Monoid          as Import (Monoid (mappend, mempty, mconcat))
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
