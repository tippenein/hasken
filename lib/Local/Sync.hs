module Local.Sync where

import           Data.Text       (pack, unpack)
import           Local.Document  (Document (..))
import qualified Remote.Client   as Client
import qualified Remote.Database as DB
import Database.Persist.Sqlite
import System.IO.Unsafe (unsafePerformIO)
import Config

uKey = unsafePerformIO $ userKey <$> remoteConfig

createDoc d = Client.createDocument doc
  where
    doc = DB.Document {
        DB.documentUserKey = pack $ uKey
      , DB.documentTitle = pack $ title d
      , DB.documentContent = pack $ content d
      , DB.documentTags = fmap pack (tags d)
      }

fromDatabaseDoc :: Entity DB.Document -> Document
fromDatabaseDoc (Entity _ d) = Document {
  title = unpack $ DB.documentTitle d,
  content = unpack $ DB.documentContent d,
  tags = fmap unpack (DB.documentTags d)
  }
