module Sync where

import           Data.Text       (pack, unpack)
import           Document        (Document (..))
import qualified Remote.Client   as Client
import qualified Remote.Database as DB

createDoc d = Client.createDocument doc
  where
    doc = DB.Document {
        DB.documentTitle = pack $ title d
      , DB.documentContent = pack $ content d
      , DB.documentTags = fmap pack (tags d)
      }

fromDatabaseDoc :: DB.Document -> Document
fromDatabaseDoc d = Document {
  title = unpack $ DB.documentTitle d,
  content = unpack $ DB.documentContent d,
  tags = fmap unpack (DB.documentTags d)
  }
