{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Remote.API where

import Data.Proxy
import Remote.Types
import Servant.API

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

type DocumentAPI =
       ListDocuments
  :<|> CreateDocument

type ListDocuments =
     "documents"
  :> Get '[JSON] DocumentResponse

type CreateDocument =
     "documents"
  :> ReqBody '[JSON] Document
  :> Post '[JSON] Document


-- separate definitions of each route will go here
-- for example:
--   type ListThings = "things"
--     :> QueryParam "someBool" Bool
--     :> Get '[JSON] Thing
