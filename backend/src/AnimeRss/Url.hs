module AnimeRss.Url
  ( Url (..)
  , fromUrl
  )
where

import Relude
import qualified Database.PostgreSQL.Simple.FromField as SQL
import qualified Database.PostgreSQL.Simple.ToField as SQL
import Network.URI

newtype Url = Url URI
  deriving stock (Show, Eq, Generic)

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
