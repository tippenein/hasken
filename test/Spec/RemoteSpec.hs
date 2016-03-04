{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Spec.RemoteSpec (spec) where

import Control.Exception   (bracket)
import Remote.Database
import Remote.Server       (app)
import System.Directory    (removeFile)
import Test.Hspec
import Test.Hspec.Wai      hiding (pending)
import Test.Hspec.Wai.JSON

seedDb = undefined
  -- runOnDb "hasken.test.db" $ mapM_ createDoc docs
removeDb = removeFile "hasken.test.db"

withTestDatabase = bracket seedDb removeDb

spec :: Spec
spec = with (return app) $ do
  describe "GET /tags" $ do
    it "responds with 200" $ do
      get "/tags" `shouldRespondWith` 200

    -- it "returns a tag cloud" $ do
    --   get "/tags" `shouldRespondWith` [json|[]|]

  describe "GET /documents" $ do
    it "responds with 200" $ do
      get "/tags" `shouldRespondWith` 200

    -- it "returns all the documents" $ do
    --   get "/documents" `shouldRespondWith` [json|[]|]

