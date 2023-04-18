{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AnimeRss.Scraper.Service (
  runScraper,
) where

import AnimeRss.DataModel.Queries (insertEpisode, selectGogoAnimeUrl, upsertGogoAnimeUrl)
import AnimeRss.Scraper.Parser.Gogoanime (getEntrisFromFronPage)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (mapM_, (>>))
import Control.Monad.Catch (handle)
import DBE (PostgreSql, withTransaction)
import Data.Function (($), (.))
import Data.Functor (void)
import Data.Int (Int)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Network.HTTP.Client hiding (hrFinalRequest)
import Network.Wreq
import Optics
import Otel.Effect
import Otel.Type
import Text.Show (show)
import Data.Semigroup ((<>))

scraperScope :: Scope "scraper" "0.0.0"
scraperScope = Scope

runScraper :: (Otel :> es, PostgreSql :> es, IOE :> es) => Int -> Eff es ()
runScraper time = handle (\exception -> logError [KeyValue "exception" . StringV . T.pack $ show @SomeException exception] "exception during the scraping" >> liftIO (threadDelay 1000000))
  . withInstrumentationScope scraperScope
  $ do
    -- TODO: Add exception handling...
    logInfo_ "requesting data from gogoanime"
    gogoAnimeUrl <- selectGogoAnimeUrl
    void . traceInternal_ "update redirects" $ do
      historiedResponse <- liftIO . customHistoriedMethod "GET" $ show gogoAnimeUrl
      let finalRequest = historiedResponse ^. lensVL hrFinalRequest
      let newPath = "https://" <> host finalRequest
      logInfo [KeyValue "new_url" . StringV $ T.decodeUtf8 newPath] "inserting new gogoAnimeUrl"
      upsertGogoAnimeUrl $ T.decodeUtf8 newPath
    entries <- traceInternal_ "running scraper" $ do
      liftIO $ getEntrisFromFronPage gogoAnimeUrl
    logInfo_ "data from gogoanime received"
    traceInternal_ "inserting scraped data into DB" $ do
      mapM_ (withTransaction . insertEpisode) entries
    logInfo_ "scrapper finished"
    liftIO $ threadDelay time
