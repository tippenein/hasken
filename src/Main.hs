{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import           Data.Acid
import           Data.Maybe         (fromMaybe)
import           Document
import           System.Environment (getArgs, getEnv)

import           Config             (localStorageLocation)
import qualified Control.Exception  as Exception
import qualified Remote.Main        as Server
import           Sync               (doSync)

$(makeAcidic ''Database [
  'addDocument,
  'viewDocuments,
  'searchDocuments
  ])

add db doc = do
  putStrLn $ "added document: " ++ show doc
  update db (AddDocument doc)

list db limit = do
  putStrLn $ "listing last " ++ show limit ++ " documents: "
  query db (ViewDocuments limit)

search db q = do
  putStrLn $ "query on: " ++ q
  query db (SearchDocuments q)

deletePrompt :: IO ()
deletePrompt = putStrLn "asdf"

display = mapM_ displayDoc

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
    ["search", q] -> do
      documents <- search database q
      display documents
    ["delete"] -> deletePrompt
    ["sync"] -> doSync
    ["serve"] -> Server.main
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

