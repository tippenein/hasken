{-# LANGUAGE OverloadedStrings #-}
module Remote.Server (runServer) where

import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Trans.Either
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors          (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import Remote.API                           as API
import Remote.Database                      as Database
import Remote.Types

-- | helper type for handling the default servant response
type Handler a = EitherT ServantErr IO a

server :: Server DocumentAPI
server =
       listDocuments
  :<|> createDocument

listDocuments = do
  documents <- liftIO $ Database.selectDocuments(100)
  return $ DocumentResponse documents

createDocument doc = do
  liftIO $ Database.insertDocument doc
  return doc

middlewares = simpleCors . logStdout

app :: Application
app = middlewares (serve API.documentAPI server)

runServer :: Port -> IO ()
runServer port = run port app
