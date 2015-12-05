{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import Data.Acid
import Data.Maybe              (fromMaybe)
import Document
import System.Directory        (getHomeDirectory)
import System.Environment      (getArgs, getEnv)
import System.FilePath         (joinPath)
import Text.PrettyPrint.Leijen (text)

$(makeAcidic ''Database [
  'addDocument,
  'viewDocuments,
  'searchDocuments
  ])

storageLocation :: IO FilePath
storageLocation = do
  homePath <- getHomeDirectory
  return $ joinPath [homePath, ".hasken_store"]

display :: [Document] -> IO ()
display = mapM_ print

deletePrompt :: IO ()
deletePrompt = putStrLn "asdf"

main :: IO ()
main = do
  args <- getArgs
  loc <- storageLocation
  database <- openLocalStateFrom loc (Database [])
  case args of
    ["help"] -> putStrLn usage
    ["add", title, tags, content] -> do
      update database $ AddDocument (buildDocument [title, tags, content])
      putStrLn "Your document has been added to the database."
    ["search", q] -> do
      documents <- query database (SearchDocuments q)
      putStrLn $ "document query on: " ++ q
      display documents
    ["delete"] -> deletePrompt
    [] -> do
      putStrLn "Last 10 documents:"
      documents <- query database (ViewDocuments 10)
      display documents
    _  -> putStrLn usage

usage = unlines [ "usage:"
                , "  add title tag1,tag2,tag3 content and stuff"
                , "  search <term>"
                , "----"
                , "no argument gives you the last 10 documents added"]

