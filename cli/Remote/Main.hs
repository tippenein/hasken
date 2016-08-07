module Remote.Main where

import qualified Control.Exception       as Exception
import           Control.Monad           (liftM)
import           Database.Persist.Sqlite (runMigration)
import           Remote.Database         (migrateAll, runDB)
import           Remote.Server           (runServer)
import           System.Environment      (getEnv)

main :: IO ()
main = do
  runDB $ runMigration migrateAll

  port <- liftM read $ getEnv "PORT"
  putStrLn ("Starting on port " ++ show port ++ "...")
  Exception.catch
    (runServer port)
    (\ Exception.UserInterrupt -> putStrLn "\nStopping...")
