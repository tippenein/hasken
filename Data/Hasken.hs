{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hasken where

import Data.Acid
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import GHC.Generics

data Document = Document { title :: String
                         , content :: String
                         , tags :: [String]
                         } deriving (Typeable, Generic)

data Database = Database [Document]

instance Show Document where
  show (Document a b c) = a ++ " - " ++ show b

instance SafeCopy Document where
  putCopy Document{..} = contain $ do safePut title; safePut content; safePut tags
  getCopy = contain $ Document <$> safeGet <*> safeGet <*> safeGet

$(deriveSafeCopy 0 'base ''Database)

-- Transactions are defined to run in either the 'Update' or 'Query' monad
addDocument :: Document -> Update Database ()
addDocument doc = do
  Database documents <- get
  put $ Database (doc:documents)

viewDocuments :: Int -> Query Database [Document]
viewDocuments limit = do
  Database documents <- ask
  return $ take limit documents

searchDocuments :: String -> Query Database [Document]
searchDocuments query = do
  Database documents <- ask
  return $ filter filterDoc documents
  where
    filterDoc document =
      (query `isInfixOf` title document) ||
      (query `isInfixOf` content document) ||
      any (isInfixOf query) (tags document)

-- This defines @ViewDocuments@, @AddDocument@, etc for us
$(makeAcidic ''Database ['addDocument, 'viewDocuments, 'searchDocuments])

buildDocument :: [String] -> Document
buildDocument args = Document {title = _title, tags = _tags, content = _content}
  where
    _title   = head args
    _tags    = splitOn "," (head $ tail args)
    _content = unwords $ tail $ tail args
