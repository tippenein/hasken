{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import qualified Control.Exception  as Exception
import           Data.Acid
import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs, getEnv)

import           Config             as Config
import qualified Control.Exception  as Exception
import           Local.Document
import           Local.Sync         (createDoc, fromDatabaseDoc)
import qualified Remote.Client      as Client
import qualified Remote.Main        as Server

$(makeAcidic ''Database [
  'addDocument,
  'viewDocuments,
  'searchDocuments
  ])


upsert docs database = undefined
  -- fmap (\doc -> update database (AddDocument (fromDatabaseDoc doc))) docs

doSync localDocs database = do
  mapM_ createDoc localDocs
  remoteDocs <- Client.listDocuments
  -- upsert remoteDocs database
  createCheckpoint database
  putStrLn $ "synced: " ++ show remoteDocs

add db doc = do
  putStrLn $ "added document: " ++ show doc
  update db (AddDocument doc)

list db limit = do
  putStrLn $ "listing last " ++ show limit ++ " documents: "
  query db (ViewDocuments limit)

deletePrompt :: IO ()
deletePrompt = putStrLn "asdf"

display :: [Document] -> IO ()
display docs = do
  showTagsP <- showTags <$> Config.localConfig
  if showTagsP
     then mapM_ displayDocWithTags docs
     else mapM_ displayDoc docs

main :: IO ()
main = do
  args <- getArgs
  loc <- localStorageLocation
  database <- openLocalStateFrom loc (Database [])
  case args of
    ["help"] -> putStrLn usage
    ["add", title, tags, content] -> do
      let newDoc = buildDocument $ [title, tags, content]
      add database newDoc
    ("search" : qs) -> do
      documents <- query database (SearchDocuments qs)
      display documents
    ["delete"] -> deletePrompt
    ["sync"] -> do
      documents <- list database 1000
      doSync documents database
    ["serve"] -> do
      closeAcidState database
      Server.main
    [] -> do
      documents <- list database 10
      display documents
    _  -> putStrLn usage
  closeAcidState database

usage = unlines [ "usage:"
                , "  client functions:"
                , "    add title tag1,tag2,tag3 content and stuff"
                , "    search <term>"
                , "  remote functions:"
                , "    serve <port>"
                , "    sync"
                , "----"
                , "no argument gives you the last 10 documents added"]

