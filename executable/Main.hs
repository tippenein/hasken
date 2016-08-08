{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import qualified Control.Exception   as Exception
import           Data.Acid
import           Data.Foldable
import qualified Data.Version        as Version
import           Options.Applicative
import qualified Paths_hasken        as Meta

import           HaskenConfig
import           Local.Document
import           Local.Sync          (createDoc, fromDatabaseDoc)
import qualified Remote.Client       as Client
import qualified Remote.Main         as Server

$(makeAcidic ''Database [
  'addDocument,
  'viewDocuments,
  'searchDocuments,
  'hardUpdate,
  'allDocuments
  ])


display :: [Document] -> IO ()
display docs = do
  showTagsSetting <- showTags <$> localConfig
  if showTagsSetting
     then traverse_ displayDocWithTags docs
     else traverse_ displayDoc docs

showTagsParser :: Parser Bool
showTagsParser =
  switch (
    long "show-tags" <> help "show tags when displaying documents" )

optParser :: Parser Options
optParser =
  Options
  <$> switch (long "version" <> help "Show version and exit")
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

commandParser :: Parser (Maybe Command)
commandParser = optional $ subparser $
     command "add" (parseAdd `withInfo` "Add a document to the local storage")
  <> command "search" (parseSearch `withInfo` "search the local docs")
  <> command "list" (parseList `withInfo` "list local docs (defaults to limit 10)")
  <> command "sync" (pure Sync `withInfo` "sync with the remote")
  <> command "serve" (pure Serve `withInfo` "serve an instance of the remote component")

data Options
  = Options
  { optShowVersion :: Bool
  , optShowTags    :: Bool
  , optCommand     :: Maybe Command }
  deriving (Show)

data Command
  = Add [String]
  | Search [String]
  | List (Maybe String)
  | Sync
  | Serve
  deriving (Show)

run :: Options -> IO ()
run opts =
  if optShowVersion opts
  then showVersion
  else run' opts


showVersion :: IO ()
showVersion = putStrLn $ "Version " <> Version.showVersion Meta.version

withLocalDatabase :: (AcidState Database -> IO ()) -> IO ()
withLocalDatabase = Exception.bracket openDb closeAcidState
  where
    openDb = do
      loc <- localStorageLocation
      openLocalStateFrom loc (Database [])

showHelpText :: ParserPrefs -> ParserInfo a -> IO ()
showHelpText pprefs pinfo = handleParseResult . Failure $
  parserFailure pprefs pinfo ShowHelpText mempty

-- defaultPrefs = ParserPrefs
--       { prefMultiSuffix = ""
--       , prefDisambiguate = False
--       , prefShowHelpOnError = False
--       , prefBacktrack = True
--       , prefColumns = 80 }

run' :: Options -> IO ()
run' opts =
  case optCommand opts of
    Nothing -> showHelpText defaultPrefs parserOpts
    Just cmd -> processCmd cmd

processCmd :: Command -> IO ()
processCmd Serve = Server.main
processCmd cmd = withLocalDatabase $ \database ->
  case cmd of
    Add passedArgs -> do
      let newDoc = buildDocument passedArgs
      update database (AddDocument newDoc)
    Search qs ->
      query database (SearchDocuments qs) >>= display
    Sync -> do
      documents <- query database AllDocuments
      doSync documents database
    List i ->
      case i of
        Nothing -> query database (ViewDocuments 10) >>= display
        Just n -> query database (ViewDocuments (read n :: Int)) >>= display

addNew remoteDocs database = update database (HardUpdate docs')
  where
    docs' = fmap fromDatabaseDoc remoteDocs

doSync :: Foldable t => t Document -> AcidState Database -> IO ()
doSync localDocs database = do
  traverse_ createDoc localDocs
  remoteDocs <- Client.listDocuments Nothing
  addNew remoteDocs database
  createCheckpoint database
  putStrLn $ "synced successfully " ++ show (length remoteDocs) ++ " documents"

main :: IO ()
main = execParser parserOpts >>= run

parserOpts :: ParserInfo Options
parserOpts =
  info (helper <*> optParser)
  ( fullDesc
  <> progDesc "tagged local storage with a sync option"
  <> header "create and tag documents for searchable recall"
  )
