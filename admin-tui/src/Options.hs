module Options (Configuration (..), options) where

import Options.Applicative
import Relude

data Configuration = Configuration
  { databaseConnectionString :: String
  }

optionsParser :: Parser Configuration
optionsParser =
  Configuration
    <$> strOption
      ( long "database-connection"
          <> metavar "DATABASE_SERVER"
          <> help "psql connection string to the database"
      )

options :: ParserInfo Configuration
options =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Administration interface for Anime RSS used for configuration\
          \ of users and initial passwords."
        <> header "Administration interface for Anime RSS"
    )
