{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module DocumentStore.API () where

import Data.Proxy
import Servant.API
import DocumentStore.Types

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

type DocumentAPI =
       ListDocuments
  :<|> ShowDocument

-- separate definitions of each route will go here
-- for example:
--   type ListThings = "things"
--     :> QueryParam "someBool" Bool
--     :> Get '[JSON] Thing
