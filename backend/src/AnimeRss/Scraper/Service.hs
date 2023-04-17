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
{-# LANGUAGE BlockArguments #-}

module AnimeRss.Scraper.Service (
  runScraper,
) where

import AnimeRss.DataModel.Queries (insertEpisode, selectGogoAnimeUrl, upsertGogoAnimeUrl)
import AnimeRss.Scraper.Parser.Gogoanime (getEntrisFromFronPage)
import Control.Concurrent (threadDelay)
import Control.Monad (mapM_)
import DBE (PostgreSql, withTransaction)
import Data.Function (($), (.))
import Data.Int (Int)
import Effectful (Eff, IOE, liftIO, (:>))
import Otel.Effect
import Otel.Type
import Control.Monad.Catch (handle)
import Text.Show (show)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Control.Exception (SomeException)
import Network.Wreq
import Optics
import Data.Functor (void)
import Network.HTTP.Client hiding (hrFinalRequest)

scraperScope :: Scope "scraper" "0.0.0"
scraperScope = Scope

runScraper :: (Otel :> es, PostgreSql :> es, IOE :> es) => Int -> Eff es ()
runScraper time = handle (\exception -> logError [KeyValue "exception" . StringV . T.pack $ show @SomeException  exception] "exception during the scraping")
  . withInstrumentationScope scraperScope $ do
    -- TODO: Add exception handling...
    logInfo_ "requesting data from gogoanime"
    gogoAnimeUrl <- selectGogoAnimeUrl
    void . traceInternal_ "update redirects" $ do
      historiedResponse <- liftIO . customHistoriedMethod "GET" $ show gogoAnimeUrl
      let finalRequest = historiedResponse ^. lensVL hrFinalRequest
      let newPath = path finalRequest
      upsertGogoAnimeUrl $ T.decodeUtf8 newPath
    entries <- traceInternal_ "running scraper" $ do
      liftIO $ getEntrisFromFronPage gogoAnimeUrl
    logInfo_ "data from gogoanime received"
    traceInternal_ "inserting scraped data into DB" $ do
      mapM_ (withTransaction . insertEpisode) entries
    logInfo_ "scrapper finished"
    liftIO $ threadDelay time
