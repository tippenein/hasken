{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Remote.Types where

import Data.Aeson
import Data.SafeCopy
import Data.Text
import Data.Typeable
import GHC.Generics

data Document = Document {
    title   :: Text
  , content :: Text
  , tags    :: [Text]
  } deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance SafeCopy Document where
  putCopy Document{..} = contain $ do
    safePut title; safePut content; safePut tags
  getCopy = contain $ Document <$> safeGet <*> safeGet <*> safeGet

data DocumentResponse = DocumentResponse {
    documents :: [Document]
  } deriving (Show, Generic, ToJSON, FromJSON)
