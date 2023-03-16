{-# LANGUAGE QuasiQuotes #-}

module AnimeRss.DataModel.Queries
  ( insertDbUser
  , getDbUserById
  , listDbUsers
  , deleteDbUser
  , insertEpisode
  , listAnimes
  )
where

import AnimeRss.DataModel.Types
import AnimeRss.Ids
import Control.Monad.Catch
import DBE
import qualified Database.PostgreSQL.Simple as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Relude hiding (head, id)
import Data.List
-- import Relude.Unsafe (head)
--
-- expecOneResult :: Text -> [a] -> Eff es a
-- expecOneResult origin values = do
--   let len = length values
--   let err = unexpectedAmountOfResults origin 1 1 $ len
--   if len == 1
--     then pure $ head values
--     else throwM err

expecOneOrZeroResults :: Text -> [a] -> Eff es (Maybe a)
expecOneOrZeroResults origin values = do
  let len = length values
  let err = unexpectedAmountOfResults origin 0 1 len
  if len < 2
    then pure $ listToMaybe values
    else throwM err

expectOneAction :: Text -> Int64 -> Eff es ()
expectOneAction origin count = do
  let err = unexpectedAmountOfActions origin 0 1 count
  if count == 1
    then pure ()
    else throwM err

expectOneResult :: Text -> [a] -> Eff es a
expectOneResult origin values = do
  let len = length values
  let err = unexpectedAmountOfResults origin 1 1 len
  if len == 1
    then pure $ head values
    else throwM err

insertDbUser :: (PostgreSql :> es) => CreateUser -> Eff es (Maybe User)
insertDbUser CreateUser {..} = do
  ret <-
    returning
      [sql| INSERT INTO users (
        name,
        email,
        password
    )
    VALUES (?,?,?)
    RETURNING
        id,
        name,
        email,
        password,
        news_channel
    |]
      [(name, email, password)]
  expecOneOrZeroResults "createDbUser" ret

getDbUserById :: (PostgreSql :> es) => UserId -> Eff es (Maybe User)
getDbUserById userId = do
  ret <-
    query
      [sql| SELECT
        id,
        name,
        email,
        news_channel,
        password
    FROM users
    WHERE id = ?
    |]
      (SQL.Only userId)
  expecOneOrZeroResults "getDbUser" ret

listDbUsers :: (PostgreSql :> es) => Eff es [User]
listDbUsers = do
  query_
    [sql| SELECT
        id,
        name,
        email,
        password,
        news_channel
    FROM users
    |]

deleteDbUser :: (PostgreSql :> es) => UserId -> Eff es ()
deleteDbUser userId = do
  ret <- execute
    [sql| DELETE FROM users WHERE id = ?
    |]
    (SQL.Only userId)
  expectOneAction "deleteDBUsers" ret

insertEpisode :: (PostgreSql :> es, IOE :> es) => CreateEpisode -> Eff es ()
insertEpisode CreateEpisode{..} = do
  -- TODO: Add logging
  liftIO $ putStrLn "inserting anime"
  ret <-
    returning
      [sql| INSERT INTO animes (
        title,
        image_url,
        url
    )
    VALUES (?,?,?)
    ON CONFLICT (title) DO UPDATE SET title = excluded.title
    RETURNING
        id
    |]
      [(title, imageUrl, animeUrl)]
  animeId <- SQL.fromOnly @AnimeId <$> expectOneResult "insertAnime" ret
  liftIO $ putStrLn "inserting episode"
  ret2 <-
    execute
      [sql| INSERT INTO episodes (
        url,
        number,
        anime_id
    )
    VALUES (?,?,?)
    ON CONFLICT (url) DO UPDATE SET url = excluded.url
    |]
      (url, number, animeId)
  expectOneAction "insertAnime" ret2

listAnimes :: (PostgreSql :> es, IOE :> es) => Eff es [Anime]
listAnimes = do
  -- TODO: Add logging
  liftIO $ putStrLn "listAnimes"
  query_
      [sql| SELECT
        id,
        title,
        image_url,
        url,
        date
    FROM animes
    ORDER by date DESC
    |]
