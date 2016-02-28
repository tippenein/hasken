module HaskenSpec (spec) where

import Local.Document

import Test.Hspec
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, oneof)

spec :: Spec
spec = do
  describe "searchDocuments" $ do
    it "builds content correctly" $ do
      let doc = buildDocument ["title", "tags", "content", "separated", "by", "spaces"]
      content doc `shouldBe` "content separated by spaces"

