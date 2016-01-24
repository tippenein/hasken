module DocumentStoreSpec (spec) where

import DocumentStore

import Test.Hspec

spec :: Spec
spec =
  describe "main" $ do
    it "returns the unit" $
      main `shouldReturn` ()
