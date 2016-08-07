{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Remote.API where

import Data.Aeson
import Data.Proxy
import Data.Text               (Text)
import Database.Persist.Sqlite
import Remote.Database
import Servant.API

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy


instance FromJSON Document
instance FromJSON (Entity Document) where
  parseJSON = entityIdFromJSON
  -- parseJSON obj@(Object v) =  Entity <$> v .:  "id" <*> parseJSON obj

type DocumentAPI =
       ListDocuments
  :<|> CreateDocument

type ListDocuments =
     "documents"
  :> Capture "user_key" Text
  :> QueryParam "q" Text
  :> Get '[JSON] [Entity Document]

type CreateDocument =
     "documents"
  :> ReqBody '[JSON] Document
  :> Post '[JSON] (Entity Document)

