{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HaskenConfig where

import Data.Yaml        as Yaml
import GHC.Generics
import System.Directory (getHomeDirectory)
import System.FilePath  (joinPath)

localStorageLocation :: IO FilePath
localStorageLocation = do
  homePath <- getHomeDirectory
  pure (joinPath [homePath, ".hasken_store"])

configPath :: IO FilePath
configPath = do
  homePath <- getHomeDirectory
  pure $ joinPath [homePath, ".hasken.yml"]

localConfig :: IO LocalConfig
localConfig = do
  c <- readConfig <$> configPath
  local <$> c

remoteConfig :: IO RemoteConfig
remoteConfig = do
  c <- readConfig <$> configPath
  remote <$> c

readConfig :: FilePath -> IO HaskenConfig
readConfig path = do
  maybeConf <- Yaml.decodeFile path :: IO (Maybe HaskenConfig)
  case maybeConf of
    Nothing -> error "Could not parse config file."
    Just conf -> pure conf

data HaskenConfig = HaskenConfig
  { remote :: RemoteConfig
  , local  :: LocalConfig
  } deriving (Show, Generic, FromJSON)

data RemoteConfig = RemoteConfig
  { domain  :: String
  , port    :: Int
  , userKey :: String
  } deriving (Show, Generic, FromJSON)

data LocalConfig = LocalConfig
  { showTags :: Bool
  } deriving (Show, Generic, FromJSON)
