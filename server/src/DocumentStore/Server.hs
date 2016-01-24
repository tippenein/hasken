{-# LANGUAGE OverloadedStrings #-}
module DocumentStore.Server (runServer) where

import Control.Monad.Trans.Either
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import DocumentStore.API as API
import DocumentStore.Types

-- | helper type for handling the default servant response
data Handler a = EitherT ServantErr IO a

server :: Server DocumentAPI
server = undefined --the definition of handlers

app :: Application
app = serve API.documentApi server

runServer :: Port -> IO ()
runServer port = run port app
