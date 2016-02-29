{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Remote.API where

import Data.Proxy
import Remote.Database
import Servant.API

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

type DocumentAPI =
       ListDocuments
  :<|> CreateDocument

type ListDocuments =
     "documents"
  :> Get '[JSON] [Document]

type CreateDocument =
     "documents"
  :> ReqBody '[JSON] Document
  :> Post '[JSON] Document
