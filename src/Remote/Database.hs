{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Remote.Database
  (
    insertDocument
  , selectDocuments
  ) where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.SafeCopy

import Document             (Document)

data Database = Database [Document]
$(deriveSafeCopy 0 'base ''Database)

insertDoc :: Document -> Update Database ()
insertDoc doc = do
  Database documents <- get
  put (Database (doc:documents))

selectDoc :: Int -> Query Database [Document]
selectDoc limit = do
  Database documents <- ask
  return (take limit documents )

$(makeAcidic ''Database ['insertDoc, 'selectDoc])

insertDocument :: Document -> IO ()
insertDocument doc = do
  database <- openLocalState (Database [])
  update database (InsertDoc doc)
  closeAcidState database
  putStrLn $ "saved document" ++ show doc

selectDocuments :: Int -> IO [Document]
selectDocuments limit = do
  database <- openLocalState (Database [])
  putStrLn $ "The last " ++ show limit ++ "documents:\n" -- ++ (map show documents)
  documents <- query database (SelectDoc limit)
  closeAcidState database
  return documents
