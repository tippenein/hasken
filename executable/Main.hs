{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import qualified Control.Exception   as Exception
import           Data.Acid
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Version        as Version
import           Options.Applicative
import qualified Paths_hasken        as Meta
import           System.Environment  (getArgs, getEnv)

import           Config
import           Local.Document
import           Local.Sync          (createDoc, fromDatabaseDoc)
import qualified Remote.Client       as Client
import qualified Remote.Main         as Server

$(makeAcidic ''Database [
  'addDocument,
  'viewDocuments,
  'searchDocuments
  ])


upsert docs database = undefined
  -- fmap (\doc -> update database (AddDocument (fromDatabaseDoc doc))) docs

doSync localDocs database = do
  mapM_ createDoc localDocs
  remoteDocs <- Client.listDocuments
  -- upsert remoteDocs database
  createCheckpoint database
  putStrLn $ "synced: " ++ show remoteDocs



deletePrompt :: IO ()
deletePrompt = putStrLn "asdf"

display :: [Document] -> IO ()
display docs = do
  showTagsSetting <- showTags <$> localConfig
  if showTagsSetting
     then mapM_ displayDocWithTags docs
     else mapM_ displayDoc docs

showTagsParser :: Parser Bool
showTagsParser =
  switch (
    long "show-tags"
    <> help "show tags when displaying documents" )

optParser :: Parser Options
optParser =
  Options
  <$> switch (short 'v' <> long "version" <> help "Show version and exit")
  <*> showTagsParser
  <*> commandParser

parseAdd :: Parser Command
parseAdd =
  Add <$> some (argument str (metavar "Title Tag1,Tag2 Content and stuff"))

parseSearch :: Parser Command
parseSearch =
  Search <$> some (argument str (metavar "QUERIES.."))

parseList :: Parser Command
parseList = List <$> optional (argument str (metavar "LIMIT"))

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

commandParser :: Parser Command
commandParser =
  subparser $
       command "add" (parseAdd `withInfo` "Add a document to the local storage")
    <> command "search" (parseSearch `withInfo` "search the local docs")
    <> command "list" (parseList `withInfo` "list local docs (defaults to limit 10)")
    <> command "sync" (pure Sync `withInfo` "sync with the remote")
    <> command "serve" (pure Serve `withInfo` "serve an instance of the remote component")

data Options
  = Options
  { optShowVersion :: Bool
  , optShowTags    :: Bool
  , optCommand     :: Command }

data Command
  = Add [String]
  | Search [String]
  | List (Maybe String)
  | Sync
  | Serve

showVersion =
  putStrLn $ "Version " <> Version.showVersion Meta.version

run :: Options -> IO ()
run opts =
  if optShowVersion opts
  then showVersion
  else run' opts

withLocalDatabase = Exception.bracket openDb closeAcidState
  where
    openDb = do
      loc <- localStorageLocation
      openLocalStateFrom loc (Database [])

run' :: Options -> IO ()
run' opts = withLocalDatabase $ \database ->
  case optCommand opts of
    Add passedArgs -> do
      let newDoc = buildDocument passedArgs
      update database (AddDocument newDoc)
    Search qs ->
      query database (SearchDocuments qs) >>= display
    Sync -> do
      documents <- query database (ViewDocuments 1000)
      doSync documents database
    Serve -> do
      closeAcidState database
      Server.main
    List i ->
      case i of
        Nothing -> query database (ViewDocuments 10) >>= display
        Just n -> query database (ViewDocuments (read n :: Int)) >>= display

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info (helper <*> optParser)
      ( fullDesc
      <> progDesc "tagged local storage with a sync option"
      <> header "create and tag documents for searchable recall"
      )
