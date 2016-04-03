{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either
import Servant                    hiding (host)
import Servant.Client             hiding (host)
import System.IO.Unsafe           (unsafePerformIO)
import Data.Text as T

import qualified Config
import Remote.API

type Action a = EitherT ServantError IO a

run :: Action a -> IO a
run action = do
  result <- runEitherT action
  case result of
    Left message -> error (show message)
    Right x -> return x

{-# NOINLINE ukey #-}
ukey = unsafePerformIO $ Config.userKey <$> Config.remoteConfig

makeBaseUrl :: IO BaseUrl
makeBaseUrl = do
  h <- Config.domain <$> Config.remoteConfig
  p <- Config.port <$> Config.remoteConfig
  return $ BaseUrl Http h p

listDocuments' :<|> createDocument' =
  client documentAPI (unsafePerformIO makeBaseUrl)

listDocuments = run $ listDocuments' (T.pack ukey)
createDocument doc = run $ createDocument' doc
