module LambdaCms.Core.Settings where

import           Data.Default               (def)
import           Language.Haskell.TH.Syntax
import           Prelude
import           Text.Hamlet
import           Yesod.Default.Util

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
            { hamletNewlines = AlwaysNewlines
            }
    }

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload widgetFileSettings
