module Local.Config where

import System.Directory (getHomeDirectory)
import System.FilePath  (joinPath)

localStorageLocation :: IO FilePath
localStorageLocation = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".hasken_store"])
