{-# LANGUAGE NoImplicitPrelude #-}

module Parser.Type
    ( AnimeEntry(..)
    )
  where

import Data.String (String)
import Network.URI (URI)
import Text.Show (Show)

data AnimeEntry = AnimeEntry
    { title :: String
    , url :: URI
    , imageUrl :: String
    }
  deriving (Show)
