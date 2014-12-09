module LambdaCms.Core.Settings where

import           Data.Default               (def)
import           Language.Haskell.TH.Syntax
import           Prelude
import           Text.Coffee
import           Text.Hamlet
import           Yesod.Default.Util

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
            { hamletNewlines = AlwaysNewlines
            }
    , wfsLanguages = \hset -> defaultTemplateLanguages hset ++
                              [ TemplateLanguage True "coffee" coffeeFile coffeeFileReload
                              ]
    }

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload widgetFileSettings
