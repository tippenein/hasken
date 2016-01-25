{-# LANGUAGE OverloadedStrings #-}
module Remote.Client where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Either
import Data.Text                  (Text)
import Servant                    hiding (host)
import Servant.Client             hiding (host)
import System.IO.Unsafe           (unsafePerformIO)

import Config
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
  h <- host <$> remote <$> readConfig
  p <- port <$> remote <$> readConfig
  return $ BaseUrl Http h p

listDocuments' :<|> createDocument' =
  client documentAPI (unsafePerformIO makeBaseUrl)

listDocuments = run listDocuments'
createDocument doc = run $ createDocument' doc
