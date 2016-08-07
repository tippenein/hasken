{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Control.Monad.IO.Class     (liftIO)
import Servant                    hiding (host)
import Servant.Client             hiding (host)
import System.IO.Unsafe           (unsafePerformIO)
import Data.Text as T

import qualified Config
import Remote.API


{-# NOINLINE ukey #-}
ukey = unsafePerformIO $ Config.userKey <$> Config.remoteConfig

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

listDocuments mq = run $ listDocuments' (T.pack ukey) mq

createDocument doc = run $ createDocument' doc
