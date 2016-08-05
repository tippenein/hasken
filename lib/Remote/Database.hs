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
  , Entity
  ) where

import Control.Monad.IO.Class       (liftIO)
import Data.Aeson
import Data.Text                    (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics                 (Generic)
import System.Environment           (getEnv)


runDB q = do
  d <- liftIO envDb
  runSqlite d q

envDb :: IO Text
envDb = do
  e <- getEnv "HASKEN_ENV"
  case e of
    "test" -> pure "hasken.test.db"
    "prod" -> pure "hasken.db"
    _      -> pure "hasken.dev.db"

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


-- TODO: Fix tag querying to use Esqueleto or something NOT IN-MEMORY
selectDocuments :: Text -> Maybe Text -> IO [Entity Document]
selectDocuments userKey query = do
  docs <- runDB $ selectList [DocumentUserKey ==. userKey] []
  case query of
    Nothing -> pure docs
    Just q -> pure $ filter (\a -> q `elem` documentTags (entityVal a)) docs

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
