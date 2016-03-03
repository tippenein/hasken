module Spec.LocalSpec (spec) where

import Local.Document

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "searchDocuments" $ do
    it "builds content correctly" $ do
      let doc = buildDocument ["title", "tags", "content", "separated", "by", "spaces"]
      content doc `shouldBe` "content separated by spaces"

    it "searches on title" $ do
      let doc1 = Document "title1" "content1" ["tag1", "tag2"]
      let doc2 = Document "title2" "content2" ["tag1", "tag2"]
      let result = searchDoc ["title"] [doc1, doc2]
      result `shouldContain` [doc1, doc2]

    it "searches by tag" $ do
      let doc1 = Document "a" "c1" ["tag1", "tag2"]
      let doc2 = Document "b" "c2" ["tag1"]
      let result = searchDoc ["tag"] [doc1, doc2]
      result `shouldContain` [doc1, doc2]

    it "searches by content" $ do
      let doc1 = Document "a" "some distinct stuff" []
      let doc2 = Document "b" "not the same" []
      let result = searchDoc ["distinct"] [doc1, doc2]
      result `shouldContain` [doc1]
