{-# LANGUAGE QuasiQuotes #-}

module AnimeRss.DataModel.Queries (
  insertDbUser,
  getDbUserById,
  listDbUsers,
  deleteDbUser,
  insertEpisode,
  listAnimes,
  selectUserByEmail,
  listEpisodesByChannelId,
  insertUserFollows,
  deleteUserFollows,
  listUserRelatedAnime,
  selectUserBySession,
  insertUserSession,
) where

import AnimeRss.DataModel.Types
import AnimeRss.Ids
import AnimeRss.Rest.Api (SubParam (..))
import Control.Monad.Catch
import DBE
import Data.List hiding (null)
import Data.Text (null)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple qualified as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Relude hiding (All, head, id, null)

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
        password,
        news_channel
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
  ret <-
    execute
      [sql| DELETE FROM users WHERE id = ?
    |]
      (SQL.Only userId)
  expectOneAction "deleteDBUsers" ret

insertEpisode :: (PostgreSql :> es, IOE :> es) => CreateEpisode -> Eff es ()
insertEpisode CreateEpisode {..} = do
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

selectUserByEmail :: (PostgreSql :> es) => Text -> Eff es (Maybe User)
selectUserByEmail email = do
  ret <-
    query
      [sql| SELECT
        id,
        name,
        email,
        password,
        news_channel
    FROM users
    WHERE email = ?
    |]
      (SQL.Only email)
  expecOneOrZeroResults "getDbUser" ret

listEpisodesByChannelId :: (PostgreSql :> es, IOE :> es) => UUID -> Eff es [Episode]
listEpisodesByChannelId channelId = do
  liftIO $ putStrLn "listEpisodesByChannelId"
  ret <-
    query
      [sql| SELECT
        id
    FROM users
    WHERE news_channel = ?
    |]
      (SQL.Only channelId)
  userId <- expectOneResult @(SQL.Only UUID) "selectUserIdByChannelId" ret
  query
    [sql| SELECT
        a.title,
        e.url,
        e.number,
        a.image_url,
        e.date
    FROM episodes e
    INNER JOIN animes a
        ON e.anime_id = a.id
    INNER JOIN user_follows u
        ON u.anime_id = a.id
    WHERE u.user_id = ?
    ORDER BY e.date DESC
    LIMIT 500
    |]
    userId

insertUserFollows :: (PostgreSql :> es, IOE :> es) => CreateUserFollows -> Eff es ()
insertUserFollows CreateUserFollows {..} = do
  liftIO $ putStrLn "insertUserFollows"
  void $
    execute
      [sql| INSERT INTO user_follows (
        anime_id,
        user_id
    )
    VALUES (?,?)
    ON CONFLICT DO NOTHING
    |]
      (animeId, userId)

deleteUserFollows :: (PostgreSql :> es, IOE :> es) => DeleteUserFollows -> Eff es ()
deleteUserFollows DeleteUserFollows {..} = do
  liftIO $ putStrLn "deleteUserFollows"
  void $
    execute
      [sql| DELETE FROM user_follows WHERE user_id = ? AND anime_id = ?
    |]
      (userId, animeId)

listUserRelatedAnime :: (PostgreSql :> es, IOE :> es) => UserId -> SubParam -> Maybe Text -> Eff es [UserRelatedAnime]
listUserRelatedAnime userId subParam mSearch = do
  liftIO $ putStrLn "listUserRelatedAnime"
  liftIO $ print select
  case mNormalizedSearch of
    Just search -> query select (userId, search)
    Nothing -> query select (SQL.Only userId)
  where
    select = animeSelector <> " " <> followingSelector <> " " <> fromJoin <> " " <> order
    animeSelector =
      [sql| SELECT
        a.id,
        a.title,
        a.url,
        a.image_url,
        a.date,
      |]
    followingSelector = case subParam of
      All -> [sql| CASE WHEN uf.anime_id IS NULL THEN false ELSE true END AS following |]
      Subscribed -> [sql| True |]
      Unsubscribed -> [sql| False |]

    order = [sql| ORDER BY a.date |]
    fromJoin = case subParam of
      All ->
        [sql|
        FROM animes a
        LEFT OUTER JOIN (select anime_id from user_follows where user_id = ?) as uf
        ON a.id = uf.anime_id
        |]
          <> maybe "" (const " WHERE ") mNormalizedSearch
          <> like
      Subscribed ->
        [sql|
        FROM animes a
        LEFT JOIN user_follows uf
        ON a.id = uf.anime_id
        WHERE user_id = ?
        |]
          <> maybe "" (const " AND ") mNormalizedSearch
          <> like
      Unsubscribed ->
        [sql|
        FROM animes a
        WHERE NOT EXISTS (
          SELECT NULL
          FROM user_follows uf
          WHERE uf.user_id = ? AND a.id = uf.anime_id
        )
        |]
          <> maybe "" (const " AND ") mNormalizedSearch
          <> like
    like = case mNormalizedSearch of
      Nothing -> ""
      Just _ -> [sql| LOWER (a.title) LIKE '%' || LOWER(?) || '%' |]
    mNormalizedSearch = case mSearch of
      Just v -> if null v then Nothing else Just v
      Nothing -> Nothing

-- TODO: Session should have some timeout...
selectUserBySession :: (IOE :> es, PostgreSql :> es) => UUID -> Eff es (Maybe UserId)
selectUserBySession session = do
  liftIO $ putStrLn "selectUserBySession"
  ret <-
    query
      [sql| SELECT
        user_id
    FROM sessions
    WHERE id = ?
    |]
      (SQL.Only session)
  liftIO $ print ret
  expecOneOrZeroResults "getDbUser" $ fmap SQL.fromOnly ret

insertUserSession :: (IOE :> es, PostgreSql :> es) => UserId -> Eff es (Maybe SessionId)
insertUserSession userId = do
  liftIO $ putStrLn "insertUserSession"
  ret <-
    returning
      [sql| INSERT INTO sessions (
        user_id
    )
    VALUES (?)
    RETURNING
        id
    |]
      [SQL.Only userId]
  liftIO $ print ret
  expecOneOrZeroResults "createDbUser" $ fmap SQL.fromOnly ret
