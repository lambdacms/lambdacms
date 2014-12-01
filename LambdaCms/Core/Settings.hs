module LambdaCms.Core.Settings where

import Prelude
import Language.Haskell.TH.Syntax
import Yesod.Default.Util
import Data.Default (def)
import Text.Hamlet

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
  { wfsHamletSettings = defaultHamletSettings
      { hamletNewlines = AlwaysNewlines
      }
  }

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload widgetFileSettings
