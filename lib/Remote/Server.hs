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
import Remote.Config
import Remote.Database                      as Database


server :: Server DocumentAPI
server =
       listDocuments
  :<|> createDocument

listDocuments userKey = liftIO $ Database.selectDocuments userKey

createDocument doc = liftIO $ Database.insertDocument doc

middlewares = simpleCors . logStdout

app :: Application
app = middlewares (serve API.documentAPI server)

runServer :: Port ->  IO ()
runServer port  = run port app
