{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.I18n where

import           Data.Text
import           LambdaCms.I18n.Dutch
import           System.Locale

-- | Helper function to get the right time locale.
lambdaCmsTimeLocale :: [Text] -> TimeLocale
lambdaCmsTimeLocale ("nl":_) = dutchTimeLocale
lambdaCmsTimeLocale _        = defaultTimeLocale
