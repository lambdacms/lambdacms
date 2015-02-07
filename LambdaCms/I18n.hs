{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.I18n where

import           Data.Text
import           LambdaCms.I18n.Dutch
import           LambdaCms.I18n.Italian
import           System.Locale

-- | Helper function to get the right time locale.
lambdaCmsTimeLocale :: [Text] -> TimeLocale
lambdaCmsTimeLocale ("nl":_) = dutchTimeLocale
lambdaCmsTimeLocale ("it":_) = italianTimeLocale
lambdaCmsTimeLocale _        = defaultTimeLocale
