{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Acid
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.Data
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import GHC.Generics
import System.Environment (getArgs)

data Document = Document { title :: String
                         , content :: String
                         , tags :: [String]
                         } deriving (Show, Typeable, Data, Generic)

data Database = Database [Document]

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
      case document of
        Document a _ _ -> query `isInfixOf` a
        Document _ b _ -> query `isInfixOf` b
        Document _ _ cs -> any (query `isInfixOf`) cs

-- This defines @ViewDocuments@ and @AddDocument@ for us.
$(makeAcidic ''Database ['addDocument, 'viewDocuments])

main :: IO ()
main = do
  args <- getArgs
  database <- openLocalStateFrom "store/" (Database [Document "blub" "blub" ["tag","tag2"]])
  if null args
  then do documents <- query database (ViewDocuments 10)
          putStrLn "Last 10 documents:"
          mapM_ putStrLn [ show document | document <- documents ]
  else do update database (AddDocument (buildDocument args))
          putStrLn "Your document has been added to the database."

buildDocument :: [String] -> Document
buildDocument args = Document {title = _title, tags = _tags, content = _content}
  where
    _title = head args
    _tags = splitOn "," (head $ tail args)
    _content = head $ tail args

