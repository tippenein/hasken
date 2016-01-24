{-# LANGUAGE OverloadedStrings #-}
module DocumentStore.Server (runServer) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant

import DocumentStore.API as API
import DocumentStore.Types
import DocumentStore.Database as Database

-- | helper type for handling the default servant response
-- data Handler a = EitherT ServantErr IO a

server :: Server DocumentAPI
server =
       listDocuments
  :<|> createDocument

listDocuments = do
  documents <- liftIO $ Database.selectDocuments(100)
  return $ DocumentResponse documents

createDocument doc = do
  liftIO $ Database.insertDocument doc
  return $ doc

app :: Application
app = logStdout (serve API.documentAPI server)

runServer :: Port -> IO ()
runServer port = run port app
