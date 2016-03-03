{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Config where

import Data.Aeson       (FromJSON)
import Data.Text        (Text)
import Data.Yaml        as Yaml
import GHC.Generics
import System.Directory (getHomeDirectory)
import System.FilePath  (joinPath)

localStorageLocation :: IO FilePath
localStorageLocation = do
  homePath <- getHomeDirectory
  return (joinPath [homePath, ".hasken_store"])

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
  } deriving (Show, Generic, FromJSON)

data LocalConfig = LocalConfig
  { showTags :: Bool
  } deriving (Show, Generic, FromJSON)
