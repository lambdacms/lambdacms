{-# LANGUAGE OverloadedStrings #-}
module LambdaCms.I18n where

import           Data.Text              (Text)
import           Data.Time.Format       (TimeLocale, defaultTimeLocale)
import           Data.Time.Format.Human (HumanTimeLocale)
import           LambdaCms.Core.Message (CoreMessage)
import           LambdaCms.I18n.Default (defaultHumanTimeLocale)
import           LambdaCms.I18n.Dutch
import           LambdaCms.I18n.Italian
import           LambdaCms.I18n.Russian


-- | Provide 'HumanTimeLocale' according to given language list.
lambdaCmsSelectHumanTimeLocale ::  [Text]
                               -- ^ accept language list
                               -> (CoreMessage -> String)
                               -- ^ message render function
                               -> HumanTimeLocale
lambdaCmsSelectHumanTimeLocale ("nl":_) = dutchHumanTimeLocale
lambdaCmsSelectHumanTimeLocale ("it":_) = italianHumanTimeLocale
lambdaCmsSelectHumanTimeLocale ("ru":_) = russianHumanTimeLocale
lambdaCmsSelectHumanTimeLocale (_:rest) = lambdaCmsSelectHumanTimeLocale rest
lambdaCmsSelectHumanTimeLocale _        = defaultHumanTimeLocale
