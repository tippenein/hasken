{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Remote.API where

import Data.Proxy
import Data.Text               (Text)
import Database.Persist.Sqlite
import Remote.Database
import Servant.API

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

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

