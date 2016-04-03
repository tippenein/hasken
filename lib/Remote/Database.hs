{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Remote.Database
  ( insertDocument
  , selectDocuments
  , Document(..)
  , runDB
  , migrateAll
  ) where

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Int                     (Int64)
import Data.Text                    (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment (getEnv)
import GHC.Generics                 (Generic)
import Remote.Config
import Servant.Server               (ServantErr)


runDB = runSqlite "hasken.dev.db"

envDb = do
  e <- getEnv "HASKEN_ENV"
  case e of
    "dev"  -> return "hasken.dev.db"
    "test" -> return "hasken.test.db"
    "prod" -> return "hasken.db"
    _      -> return "hasken.dev.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Document json
    userKey Text
    title Text
    content Text
    tags [Text]
    UniqueTitleAndContent title content
    deriving Eq Show Generic
|]

selectDocuments :: Text -> IO [Document]
selectDocuments userKey = do
  docs <- runDB $ selectList [DocumentUserKey ==. userKey] []
  return $ map entityVal docs


insertDocument :: Document -> IO Document
insertDocument doc = do
  runDB $ insert_ doc
  return doc
