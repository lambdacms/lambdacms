{-# LANGUAGE OverloadedStrings #-}

module LambdaCms.I18n where

import           Data.Text
import           Data.Time.Format (defaultTimeLocale, TimeLocale)
import           Data.Time.Format.Human (HumanTimeLocale)
import           LambdaCms.Core.Message (CoreMessage)
import           LambdaCms.I18n.Default (defaultHumanTimeLocale)
import           LambdaCms.I18n.Dutch
import           LambdaCms.I18n.Italian
import           LambdaCms.I18n.Russian


-- | Helper function to get the right time locale.
lambdaCmsTimeLocale :: [Text] -> TimeLocale
lambdaCmsTimeLocale ("nl":_) = dutchTimeLocale
lambdaCmsTimeLocale ("it":_) = italianTimeLocale
lambdaCmsTimeLocale ("ru":_) = russianTimeLocale
lambdaCmsTimeLocale (_:rest) = lambdaCmsTimeLocale rest
lambdaCmsTimeLocale []       = defaultTimeLocale

-- | Provide 'HumanTimeLocale' according to given language list.
lambdaCmsSelectHumanTimeLocale ::  [Text]
                               -- ^ accept language list
                               -> (CoreMessage -> String)
                               -- ^ message render function
                               -> HumanTimeLocale
lambdaCmsSelectHumanTimeLocale ("ru":_) = russianHumanTimeLocale
-- When other language will be added uncomment following line.
-- lambdaCmsSelectHumanTimeLocale (_:rest) = lambdaCmsSelectHumanTimeLocale rest
lambdaCmsSelectHumanTimeLocale _        = defaultHumanTimeLocale
