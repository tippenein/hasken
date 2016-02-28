{-# LANGUAGE OverloadedStrings #-}

module Remote.Config where

import System.Environment (lookupEnv)

data Environment
  = Development
  | Production
  | Test
  deriving (Eq, Read, Show)

getEnvironment :: IO Environment
getEnvironment = do
  m <- lookupEnv "SERVANT_ENV"
  let e = case m of
        Nothing -> Development
        Just s -> read s
  return e

-- data PostgresqlConfig = PostgresqlConfig {
--     host :: Text
--   , port :: Int
--   , user :: Text
--   , pass :: Text
--   , db   :: Text
-- } deriving (Show, Generic, FromJSON)

