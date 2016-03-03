{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Local.Document where

import Control.Applicative     ((<$>), (<*>))
import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, put)
import Data.Acid
import Data.Aeson
import Data.List               (isInfixOf)
import Data.List.Split         (splitOn)
import Data.SafeCopy
import Data.Text               (Text)
import Data.Typeable
import GHC.Generics
import System.IO               (stdout)
import Text.PrettyPrint.Leijen (displayIO, displayS, linebreak, list, putDoc,
                                renderPretty, sep, text, (<+>))

data Document = Document { title   :: String
                         , content :: String
                         , tags    :: [String]
                         } deriving (Eq, Ord, Typeable, ToJSON, FromJSON, Generic)

data Database = Database [Document]

deriveSafeCopy 0 'base ''Database

displayDoc = putDoc . mainDoc
displayDocWithTags d = renderWidth 1000 (mainDoc d <+> tagDocs d)

renderWidth w x = displayIO stdout (renderPretty 0.4 w x)
mainDoc d =
  text (title d) <+>
  text "->" <+>
  text (content d) <+>
  linebreak

tagDocs d =
  text "  ~~| " <+>
  sep (fmap text (tags d)) <+>
  linebreak


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

searchDocuments :: [String] -> Query Database [Document]
searchDocuments queries = do
  Database documents <- ask
  return $ searchDoc queries documents

searchDoc :: [String] -> [Document] -> [Document]
searchDoc queries =
  filter filterDoc
    where
      filterDoc document =
        overQueries (title document) queries ||
        overQueries (content document) queries ||
        any (\a -> overQueries a queries) (tags document)

overQueries :: String -> [String] -> Bool
overQueries content searches = any (\search -> search `isInfixOf` content) searches

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

