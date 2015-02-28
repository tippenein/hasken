module Main where

import Data.Hasken
import Data.Acid
import System.Environment (getArgs)
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)

storageLocation :: IO FilePath
storageLocation = do
  homePath <- getHomeDirectory
  return $ joinPath [homePath, "hasken_store"]

main :: IO ()
main = do
  args <- getArgs
  loc <- storageLocation
  database <- openLocalStateFrom loc (Database [])
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


usage = unlines [ "usage:"
                , "  add title tag1,tag2,tag3 content and stuff"
                , "  search term"
                , "----"
                , "no argument gives you the last 10 documents added"]

