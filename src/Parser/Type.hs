module Parser.Type
    ( AnimeEntry(..)
    )
  where

data AnimeEntry = AnimeEntry
    { title :: String
    , url :: String
    , imageUrl :: String
    }
  deriving (Show)
