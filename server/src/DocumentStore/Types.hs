{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module DocumentStore.Types where

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Typeable
import Data.SafeCopy

data Document = Document {
    title :: Text
  , content :: Text
  , tags :: [Text]
  } deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance SafeCopy Document where
  putCopy Document{..} = contain $ do
    safePut title; safePut content; safePut tags
  getCopy = contain $ Document <$> safeGet <*> safeGet <*> safeGet

data DocumentResponse = DocumentResponse {
    documents :: [Document]
  } deriving (Show, Generic, ToJSON, FromJSON)
