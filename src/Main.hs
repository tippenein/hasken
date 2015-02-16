{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Acid
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy
import Data.Typeable
import Data.Data
import System.Environment (getArgs)

data Document = Document { title :: String
                         , content :: String
                         , tags :: [String]
                         } deriving (Show, Typeable, Data)

data Database = Database [Document]

$(deriveSafeCopy 0 'base ''Database)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.
addDocument :: Document -> Update Database ()
addDocument doc = do
  Database documents <- get
  put $ Database (doc:documents)

viewDocuments :: Int -> Query Database [Document]
viewDocuments limit = do
  Database documents <- ask
  return $ take limit documents

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
          where

buildDocument :: [String] -> Document
buildDocument args = undefined
