{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Text                  as T
import           Network.HTTP.Client        (defaultManagerSettings,
                                             newManager)
import           Servant
import           Servant.Client

import qualified HaskenConfig               as Config
import           Remote.API


ukey :: IO String
ukey = Config.userKey <$> Config.remoteConfig

makeBaseUrl :: IO BaseUrl
makeBaseUrl = do
  h <- Config.domain <$> Config.remoteConfig
  p <- Config.port <$> Config.remoteConfig
  pure $ BaseUrl Http h p ""

type Action a = ExceptT ServantError IO a

run action = do
  baseUrl <- makeBaseUrl
  manager <- newManager defaultManagerSettings
  result <- runExceptT $ action manager baseUrl
  case result of
    Left message -> error (show message)
    Right x -> pure x

listDocuments' :<|> createDocument' =
  client documentAPI

listDocumentsWith client_id mq = run $ listDocuments' client_id mq

listDocuments mq = do
  u <- T.pack <$> ukey
  run $ listDocuments' u mq

createDocument doc = run $ createDocument' doc
