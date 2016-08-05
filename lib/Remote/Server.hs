{-# LANGUAGE OverloadedStrings #-}
module Remote.Server (runServer) where

import Control.Monad.IO.Class               (liftIO, MonadIO)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors          (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant

import Data.Text
import Remote.API                           as API
import Remote.Database                      as Database


server :: Server DocumentAPI
server =
       listDocuments
  :<|> createDocument

listDocuments :: MonadIO m => Text -> Maybe Text -> m [Database.Entity Document]
listDocuments userKey mq = liftIO $ Database.selectDocuments userKey mq

createDocument :: MonadIO m => Document -> m (Entity Document)
createDocument doc = liftIO $ Database.insertDocument doc

middlewares :: Application -> Application
middlewares = simpleCors . logStdout

app :: Application
app = middlewares (serve API.documentAPI server)

runServer :: Port ->  IO ()
runServer port  = run port app
