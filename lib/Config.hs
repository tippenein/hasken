{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config where

import Data.Aeson       (FromJSON)
import Data.Text        (Text)
import Data.Yaml        as Yaml
import GHC.Generics
import System.Directory (getHomeDirectory)
import System.FilePath  (joinPath)
import System.IO.Unsafe

localStorageLocation :: IO FilePath
localStorageLocation = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".hasken_store"])

configPath :: IO FilePath
configPath = do
  homePath <- getHomeDirectory
  return $ joinPath [homePath, ".hasken.yml"]

localConfig :: IO LocalConfig
localConfig = do
  homePath <- getHomeDirectory
  local <$> unsafePerformIO (readConfig <$> configPath)

remoteConfig :: IO RemoteConfig
remoteConfig = do
  homePath <- getHomeDirectory
  remote <$> unsafePerformIO (readConfig <$> configPath)

readConfig :: FilePath -> IO HaskenConfig
readConfig path = do
  maybeConf <- Yaml.decodeFile path :: IO (Maybe HaskenConfig)
  case maybeConf of
    Nothing -> error "Could not parse config file."
    Just conf -> return conf

data HaskenConfig = HaskenConfig
  { remote :: RemoteConfig
  , local  :: LocalConfig
  } deriving (Show, Generic, FromJSON)

data RemoteConfig = RemoteConfig
  { domain :: String
  , port   :: Int
  , userKey :: String
  } deriving (Show, Generic, FromJSON)

data LocalConfig = LocalConfig
  { showTags :: Bool
  } deriving (Show, Generic, FromJSON)
