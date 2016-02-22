{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either
import Data.Aeson                 (FromJSON)
import Data.Text                  (Text)
import Data.Yaml                  as Yaml
import GHC.Generics
import Servant                    hiding (host)
import Servant.Client             hiding (host)
import System.IO.Unsafe           (unsafePerformIO)

import Config
import Remote.API

type Action a = EitherT ServantError IO a

data HaskenConfig = HaskenConfig
  { web :: WebConfig
  } deriving (Show, Generic, FromJSON)

data WebConfig = WebConfig
  { domain :: String
  , port   :: Int
  } deriving (Show, Generic, FromJSON)


readConfig :: FilePath -> IO HaskenConfig
readConfig path = do
  maybeConf <- Yaml.decodeFile path :: IO (Maybe HaskenConfig)
  case maybeConf of
    Nothing -> error "Could not parse config file."
    Just conf -> return conf

run :: Action a -> IO a
run action = do
  result <- runEitherT action
  case result of
    Left message -> error (show message)
    Right x -> return x

makeBaseUrl :: IO BaseUrl
makeBaseUrl = do
  h <- domain <$> web <$> readConfig "./hasken.yml"
  p <- port <$> web <$> readConfig "./hasken.yml"
  return $ BaseUrl Http h p

listDocuments' :<|> createDocument' =
  client documentAPI (unsafePerformIO makeBaseUrl)

listDocuments = run listDocuments'
createDocument doc = run $ createDocument' doc
