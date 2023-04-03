module AnimeRss.Url (
  Url (..),
  fromUrl,
) where

import Database.PostgreSQL.Simple.FromField qualified as SQL
import Database.PostgreSQL.Simple.ToField qualified as SQL
import Network.URI
import Relude

newtype Url = Url URI
  deriving stock (Eq, Generic)
  deriving newtype (Show)

instance SQL.FromField Url where
  fromField :: SQL.FieldParser Url
  fromField f mdata =
    SQL.fromField f mdata
      >>= (\uri -> maybe (SQL.returnError SQL.ConversionFailed f uri) (pure . Url) $ parseURI uri)

instance SQL.ToField Url where
  toField :: Url -> SQL.Action
  toField (Url uri) = SQL.toField $ show @String uri

fromUrl :: Url -> URI
fromUrl (Url v) = v
