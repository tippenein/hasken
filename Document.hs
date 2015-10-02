{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


module Document where

import Control.Applicative  ((<$>), (<*>))
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid
import Data.List            (isInfixOf)
import Data.List.Split      (splitOn)
import Data.SafeCopy
import Data.Typeable
import GHC.Generics

data Database = Database [Document]

data Document = Document { title   :: String
                         , content :: String
                         , tags    :: [String]
                         } deriving (Typeable, Generic)

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


buildDocument :: [String] -> Document
buildDocument args = Document {title = _title, tags = _tags, content = _content}
  where
    _title   = head args
    _tags    = splitOn "," (head $ tail args)
    _content = unwords $ tail $ tail args
