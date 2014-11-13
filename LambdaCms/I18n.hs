{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.I18n where

import Data.Text
import System.Locale
import LambdaCms.I18n.Dutch

lambdaCmsTimeLocale :: [Text] -> TimeLocale
lambdaCmsTimeLocale ("nl":_) = dutchTimeLocale
lambdaCmsTimeLocale _        = defaultTimeLocale
