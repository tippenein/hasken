{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either
import Servant                    hiding (host)
import Servant.Client             hiding (host)
import System.IO.Unsafe           (unsafePerformIO)

import Config                     as Config
import Remote.API

type Action a = EitherT ServantError IO a

run :: Action a -> IO a
run action = do
  result <- runEitherT action
  case result of
    Left message -> error (show message)
    Right x -> return x

makeBaseUrl :: IO BaseUrl
makeBaseUrl = do
  h <- domain <$> Config.remoteConfig
  p <- port <$> Config.remoteConfig
  return $ BaseUrl Http h p

listDocuments' :<|> createDocument' :<|> listTags' =
  client documentAPI (unsafePerformIO makeBaseUrl)

listDocuments = run listDocuments'
createDocument doc = run $ createDocument' doc
listTags = run listTags'
