{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config where

import Data.Aeson       (FromJSON)
import Data.Yaml        as Yaml
import GHC.Generics
import System.Directory (getHomeDirectory)
import System.FilePath  (joinPath)

localStorageLocation :: IO FilePath
localStorageLocation = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".hasken_store"])

data LocalConfig = LocalConfig {
  sync :: Bool
} deriving (Show, Generic, FromJSON)

data ServerConfig = ServerConfig {
    host :: String
  , port :: Int
} deriving (Show, Generic, FromJSON)

data HaskenConfig = HaskenConfig {
    remote :: ServerConfig
  , local  :: LocalConfig
} deriving (Show, Generic, FromJSON)

readConfig :: IO HaskenConfig
readConfig = do
  home <- getHomeDirectory
  configPath <- return $ joinPath [home, ".hasken.yml"]
  maybeConf <- Yaml.decodeFile configPath :: IO (Maybe HaskenConfig)
  case maybeConf of
    Nothing -> error "Could not parse config file."
    Just conf -> return conf
