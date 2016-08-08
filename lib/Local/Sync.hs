module Local.Sync where

import           Data.Text               (pack, unpack)
import           Database.Persist.Sqlite
import           HaskenConfig
import           Local.Document          (Document (..))
import qualified Remote.Client           as Client
import qualified Remote.Database         as DB

createDoc :: Document -> IO (Entity DB.Document)
createDoc d = do
  u <- userKey <$> remoteConfig
  Client.createDocument(DB.Document {
        DB.documentUserKey = pack u
      , DB.documentTitle = pack $ title d
      , DB.documentContent = pack $ content d
      , DB.documentTags = fmap pack (tags d)
      })

fromDatabaseDoc :: Entity DB.Document -> Document
fromDatabaseDoc (Entity _ d) = Document {
    title = unpack $ DB.documentTitle d,
    content = unpack $ DB.documentContent d,
    tags = fmap unpack (DB.documentTags d)
  }
