module LambdaCms.Core.Settings
       ( widgetFile
       , generateUUID
       , generateActivationToken
       ) where

import           Data.Default               (def)
import           Data.Text                  (Text)
import           Data.Text                  (pack)
import           Data.UUID
import           Data.UUID.V4
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

-- | UUID as Text
generateUUID :: IO Text
generateUUID = nextRandom >>= return . pack . toString

-- | Same as generateUUID, for now.
generateActivationToken :: IO Text
generateActivationToken = generateUUID
