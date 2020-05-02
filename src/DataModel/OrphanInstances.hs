{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module DataModel.OrphanInstances
    ()
  where

import Control.Arrow (left)
import Crypto.PasswordStore (HashParameters)
import Database.Persist.Types (PersistValue(PersistByteString, PersistText))
import Database.Persist.Class
    ( PersistField
        ( fromPersistValue
        , toPersistValue
        )
    )
import Database.Persist.Sql (SqlType(SqlBlob, SqlString), PersistFieldSql(sqlType))
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Data.Serialize (encode, decode)
import Data.Text (pack, unpack)
import Network.URI (URI, parseURI)
import Text.Show (show)

instance PersistFieldSql URI where
    sqlType _ = SqlString

instance PersistField URI where
    toPersistValue = PersistText . pack . show
    fromPersistValue (PersistText v) = maybe (Left "URI is corrupted") Right . parseURI $ unpack v
    fromPersistValue x = Left $ "When trying to deserialize an URI: expected PersistText, received: " <> pack (show x)

instance PersistFieldSql HashParameters where
    sqlType _ = SqlBlob

instance PersistField HashParameters where
    toPersistValue = PersistByteString . encode
    fromPersistValue (PersistByteString bs) = left (\e -> "Deserialization of HashParameters failed with error: " <> pack e) $ decode bs
    fromPersistValue x = Left $ "When trying to deserialize an HashParameters: expected PersistByteString, received: " <> pack (show x)

--    toPersistValue = PersistText . pack . show
--    fromPersistValue (PersistText v) = maybe (Left "PasswordHash is corrupted") Right . parsePasswordHash $ unpack v
--    fromPersistValue x = Left $ "When trying to deserialize an PasswordHash: expected PersistText, received: " <> pack (show x)
