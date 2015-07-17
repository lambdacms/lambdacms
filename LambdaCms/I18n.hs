{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.I18n where

import           Data.Text
import           Data.Time.Format (defaultTimeLocale, TimeLocale)
import           LambdaCms.I18n.Dutch
import           LambdaCms.I18n.Italian


-- | Helper function to get the right time locale.
lambdaCmsTimeLocale :: [Text] -> TimeLocale
lambdaCmsTimeLocale ("nl":_) = dutchTimeLocale
lambdaCmsTimeLocale ("it":_) = italianTimeLocale
lambdaCmsTimeLocale _        = defaultTimeLocale
