{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DocumentStore.Types where

import Data.Aeson
import Data.Text
import GHC.Generics

data Document = Document {
    title :: Text
  , content :: Text
  , tags :: [Text]
  } deriving (Show, Eq, ToJSON, FromJSON, Generic)

