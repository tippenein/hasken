{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Remote.Types where

import Data.Aeson
import Data.SafeCopy
import Data.Text
import Data.Typeable
import GHC.Generics

import Document      (Document)

data DocumentResponse = DocumentResponse {
    documents :: [Document]
  } deriving (Show, Generic, ToJSON, FromJSON)
