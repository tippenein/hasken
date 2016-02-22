module Sync
  (doSync)
  where

import Config        (localStorageLocation)
import Data.Acid
import Document
import Remote.Client as Client
import Remote.Types  as Types


doSync = do
  -- loc <- localStorageLocation
  -- localDB <- openLocalStateFrom loc (Database [])
  remoteDocs <- Client.listDocuments
  putStrLn $ "to be synced: " ++ show remoteDocs
