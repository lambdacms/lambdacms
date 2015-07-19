module FoundationSpec (spec) where

import           TestImport
import           LambdaCms.Core.Foundation

spec :: Spec
spec = do
    describe "Handler.Home" $ do
        context "some method" $ do
            it "loads the index and checks it looks right" $ do
                "some" `shouldBe` "some"
                