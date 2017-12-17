{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.URI.Static
    ( staticURI
    )
  where

import Control.Monad (fail)
import Data.Bool (otherwise)
import Data.Function (($))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.String (String)
import Language.Haskell.TH (Q, TExp)
import Network.URI (isURI, parseURI, URI)

staticURI :: String -> Q (TExp URI)
staticURI uri | isURI uri = [|| fromJust $ parseURI uri ||]
              | otherwise = fail $ "Invalid URI: " <> uri
