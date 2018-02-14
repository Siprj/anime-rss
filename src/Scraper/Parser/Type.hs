{-# LANGUAGE NoImplicitPrelude #-}

module Scraper.Parser.Type
    ( AnimeEntry(..)
    )
  where

import Data.Int (Int)
import Data.Text (Text)
import Network.URI (URI)
import Text.Show (Show)

data AnimeEntry = AnimeEntry
    { title :: Text
    , url :: URI
    , imageUrl :: URI
    , episodeNumber :: Int
    }
  deriving (Show)
