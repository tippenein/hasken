{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Data.Hasken
  ( main
  , Document
  )
where

import Data.Acid
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import GHC.Generics
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

data Document = Document { title :: String
                         , content :: String
                         , tags :: [String]
                         } deriving (Typeable, Generic)

data Database = Database [Document]

instance Show Document where
  show (Document a b c) = a ++ " - " ++ show b ++ " ~ " ++ show c

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

storageLocation :: IO FilePath
storageLocation = do
  homePath <- getHomeDirectory
  return $ joinPath [homePath, "hasken_store"]

main :: IO ()
main = do
  args <- getArgs
  database <- openLocalStateFrom "store/" (Database [])
  case args of
    ["help"] -> putStrLn usage
    [] -> do
      documents <- query database (ViewDocuments 10)
      putStrLn "Last 10 documents:"
      mapM_ putStrLn [ show document | document <- documents ]
    ["search", q] -> do
      documents <- query database (SearchDocuments q)
      putStrLn $ "document query on: " ++ q
      mapM_ putStrLn [ show document | document <- documents ]
    _ -> do
      update database $ AddDocument (buildDocument (tail args))
      putStrLn "Your document has been added to the database."

buildDocument :: [String] -> Document
buildDocument args = Document {title = _title, tags = _tags, content = _content}
  where
    _title   = head args
    _tags    = splitOn "," (head $ tail args)
    _content = unwords $ tail $ tail args

usage = unlines [ "usage:"
                , "  add title tag1,tag2,tag3 content and stuff"
                , "  search term"
                , "----"
                , "no argument gives you the last 10 documents added"]
