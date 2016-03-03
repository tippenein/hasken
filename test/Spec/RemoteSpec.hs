module Spec.RemoteSpec (spec) where

import           Control.Exception   (bracket)
import           Remote.Database
import           System.Directory    (removeFile)
import           Test.Hspec
import qualified Test.Hspec.Wai      as HW
import qualified Test.Hspec.Wai.JSON as HW

-- seedDb docs = runOnDb "hasken.test.db" $ mapM_ createDoc [docs]
removeDb = removeFile "hasken.test.db"

-- withDatabase = bracket seedDb removeDb

spec :: Spec
spec = do
  describe "/tags" $ do
    it "returns a tag cloud" $ do
      pending

  describe "/documents" $ do
    it "returns all the documents" $ do
      pending
