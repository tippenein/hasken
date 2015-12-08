{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Document where

import Control.Applicative     ((<$>), (<*>))
import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, put)
import Data.Acid
import Data.List               (isInfixOf)
import Data.List.Split         (splitOn)
import Data.SafeCopy
import Data.Text               (Text)
import Data.Typeable
import GHC.Generics
import Text.PrettyPrint.Leijen (linebreak, putDoc, text, (<+>))

data Database = Database [Document]

data Document = Document { title   :: String
                         , content :: String
                         , tags    :: [String]
                         } deriving (Eq, Ord, Typeable, Generic)

displayDoc d = putDoc (
  text (title d) <+>
  text "->" <+>
  text (content d) <+>
  linebreak
  )

instance Show Document where
  show (Document title content tags) = title ++ " - " ++ content

instance SafeCopy Document where
  putCopy Document{..} = contain $ do safePut title; safePut content; safePut tags
  getCopy = contain $ Document <$> safeGet <*> safeGet <*> safeGet


-- Transactions are defined to run in either the 'Update' or 'Query' monad
addDocument :: Document -> Update Database ()
addDocument doc = do
  Database documents <- get
  put $ Database (doc:documents)

removeDocument :: Document -> Update Database ()
removeDocument doc = do
  Database documents <- get
  let withoutDoc = filter (/= doc) documents
  put (Database withoutDoc)

viewDocuments :: Int -> Query Database [Document]
viewDocuments limit = do
  Database documents <- ask
  return (take limit documents)

searchDocuments :: String -> Query Database [Document]
searchDocuments query = do
  Database documents <- ask
  return (filter filterDoc documents)
  where
    filterDoc document =
      (query `isInfixOf` title document) ||
      (query `isInfixOf` content document) ||
      any (isInfixOf query) (tags document)


buildDocument :: [String] -> Document
buildDocument args = Document {title = _title, tags = _tags, content = _content}
  where
    _title   = head args
    _tags    = splitOn "," (second args)
    _content = unwords (thirdAndOn args)

thirdAndOn :: [String] -> [String]
thirdAndOn args = tail (tail args)

second :: [String] -> String
second ds = head (tail ds)

$(deriveSafeCopy 0 'base ''Database)

