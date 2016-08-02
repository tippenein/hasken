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
import Data.Aeson
import Data.Int                     (Int64)
import Data.Text                    (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics                 (Generic)
import Remote.Config
import Servant.Server               (ServantErr)
import System.Environment           (getEnv)


runDB q = do
  d <- liftIO envDb
  runSqlite d q

envDb :: IO Text
envDb = do
  e <- getEnv "HASKEN_ENV"
  case e of
    "test" -> return "hasken.test.db"
    "prod" -> return "hasken.db"
    _      -> return "hasken.dev.db"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Document
    userKey Text
    title Text
    content Text
    tags [Text]
    UniqueTitleAndContent title content
    deriving Eq Show Generic
|]

instance ToJSON Document where
  toJSON (Document _ t c ts) = object
    [ "title" .= t
    , "content" .= c
    , "tags" .= ts
    ]

instance ToJSON (Entity Document) where
  toJSON (Entity pid (Document _ t c ts)) = object
    [ "id" .= pid
    , "title" .= t
    , "content" .= c
    , "tags" .= ts
    ]


selectDocuments :: Text -> IO [Entity Document]
selectDocuments userKey = do
  docs <- runDB $ selectList [DocumentUserKey ==. userKey] []
  return docs

insertDocument :: Document -> IO (Entity Document)
insertDocument o = do
  d <- runDB $ insertBy o
  case d of
    Left existed -> putStrLn "record already exists" >> pure existed
    Right new -> putStrLn "inserted record" >> pure (Entity new o)

-- insertDb :: a -> IO a
-- insertDb o = do
--   d <- runDB $ insertBy o
--   case d of
--     Left _ -> putStrLn "record already exists" >> return o
--     Right _ -> putStrLn "inserted record" >> return o
