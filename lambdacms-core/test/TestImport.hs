module TestImport
    ( module TestImport
    , module Export
    ) where

import ClassyPrelude         as Export
import Test.Hspec            as Export


--runDB :: SqlPersistM a -> YesodExample App a
--runDB query = do
--    pool <- fmap appConnPool getTestYesod
--    liftIO $ runSqlPersistMPool query pool

--withApp :: SpecWith App -> Spec
--withApp = before $ do
--    settings <- loadAppSettings
--        ["config/test-settings.yml", "config/settings.yml"]
--        []
--        ignoreEnv
--    makeFoundation settings
