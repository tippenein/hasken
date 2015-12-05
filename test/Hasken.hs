module Hasken (spec) where

import Data.Hasken     (Document)

import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof)

spec :: Spec
spec = do
  describe "searchDocuments" $ do
    it "finds doc" $ do
      let doc = buildDocument ["title", "tags", "content"]
      content doc `shouldBe` "content"

