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
  upsertGogoAnimeUrl,
  selectGogoAnimeUrl,
) where

import AnimeRss.DataModel.Types
import AnimeRss.Ids
import AnimeRss.Rest.Api (SubParam (..))
import Control.Monad.Catch
import DBE
import Data.List hiding (null)
import Data.Text (null)
import Data.Text qualified as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple qualified as SQL
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Network.URI (URI, parseURI)
import Otel.Effect
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

insertDbUser :: (Otel :> es, PostgreSql :> es) => CreateUser -> Eff es (Maybe User)
insertDbUser CreateUser {..} = traceInternal_ "insertDbUser" $ do
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
  expecOneOrZeroResults "insertDbUser" ret

getDbUserById :: (Otel :> es, PostgreSql :> es) => UserId -> Eff es (Maybe User)
getDbUserById userId = traceInternal_ "getDbUserById" $ do
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
  expecOneOrZeroResults "getDbUserById" ret

listDbUsers :: (Otel :> es, PostgreSql :> es) => Eff es [User]
listDbUsers = traceInternal_ "listDbUsers" $ do
  query_
    [sql| SELECT
        id,
        name,
        email,
        password,
        news_channel
    FROM users
    |]

deleteDbUser :: (Otel :> es, PostgreSql :> es) => UserId -> Eff es ()
deleteDbUser userId = traceInternal_ "deleteDbUser" $ do
  ret <-
    execute
      [sql| DELETE FROM users WHERE id = ?
    |]
      (SQL.Only userId)
  expectOneAction "deleteDBUsers" ret

insertEpisode :: (Otel :> es, PostgreSql :> es) => CreateEpisode -> Eff es ()
insertEpisode CreateEpisode {..} = traceInternal_ "insertEpisode" $ do
  ret <-
    traceInternal_ "inserting anime" $
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
  ret2 <-
    traceInternal_ "inserting episode" $
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
  expectOneAction "insertEpisode" ret2

listAnimes :: (Otel :> es, PostgreSql :> es) => Eff es [Anime]
listAnimes = traceInternal_ "listAnimes" $ do
  -- TODO: Add logging
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

selectUserByEmail :: (Otel :> es, PostgreSql :> es) => Text -> Eff es (Maybe User)
selectUserByEmail email = traceInternal_ "selectUserByEmail" $ do
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
  expecOneOrZeroResults "selectUserByEmail" ret

listEpisodesByChannelId :: (Otel :> es, PostgreSql :> es) => UUID -> Eff es [Episode]
listEpisodesByChannelId channelId = traceInternal_ "listEpisodesByChannelId" $ do
  ret <-
    traceInternal_ "selectUserIdByChannelId" $
      query
        [sql| SELECT
        id
    FROM users
    WHERE news_channel = ?
    |]
        (SQL.Only channelId)
  userId <- expectOneResult @(SQL.Only UUID) "selectUserIdByChannelId" ret
  traceInternal_ "selectEpisodesByUser" $
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

insertUserFollows :: (Otel :> es, PostgreSql :> es) => CreateUserFollows -> Eff es ()
insertUserFollows CreateUserFollows {..} = traceInternal_ "insertUserFollows" $ do
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

deleteUserFollows :: (Otel :> es, PostgreSql :> es) => DeleteUserFollows -> Eff es ()
deleteUserFollows DeleteUserFollows {..} = traceInternal_ "deleteUserFollows" $ do
  void $
    execute
      [sql| DELETE FROM user_follows WHERE user_id = ? AND anime_id = ?
    |]
      (userId, animeId)

listUserRelatedAnime :: (Otel :> es, PostgreSql :> es) => UserId -> SubParam -> Maybe Text -> Eff es [UserRelatedAnime]
listUserRelatedAnime userId subParam mSearch = traceInternal_ "listUserRelatedAnime" $ do
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
selectUserBySession :: (Otel :> es, PostgreSql :> es) => UUID -> Eff es (Maybe UserId)
selectUserBySession session = traceInternal_ "selectUserBySession" $ do
  ret <-
    query
      [sql| SELECT
        user_id
    FROM sessions
    WHERE id = ?
    |]
      (SQL.Only session)
  expecOneOrZeroResults "selectUserBySession" $ fmap SQL.fromOnly ret

insertUserSession :: (Otel :> es, PostgreSql :> es) => UserId -> Eff es (Maybe SessionId)
insertUserSession userId = traceInternal_ "insertUserSession" $ do
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
  expecOneOrZeroResults "insertUserSession" $ fmap SQL.fromOnly ret

upsertGogoAnimeUrl :: (Otel :> es, PostgreSql :> es) => Text -> Eff es ()
upsertGogoAnimeUrl url = traceInternal_ "upsertGogoAnimeUrl" $ do
  ret <-
    execute
      [sql| INSERT INTO state (
        key,
        value
    )
    VALUES (?, ?)
    ON CONFLICT (key) DO UPDATE SET value = excluded.value
    |]
      ("gogoanime_url" :: Text, url)
  expectOneAction "upsertGogoAnimeUrl" ret

selectGogoAnimeUrl :: (Otel :> es, PostgreSql :> es) => Eff es URI
selectGogoAnimeUrl = traceInternal_ "insertGogoAnimeUrl" $ do
  ret <-
    query_
      [sql| SELECT value FROM state WHERE key = 'gogoanime_url'
    |]
  urlString <- expectOneResult "selectGogoAnimeUrl" $ fmap SQL.fromOnly ret
  maybe (throwM . invalidValue "insertGogoAnimeUrl" $ T.pack urlString) pure $ parseURI urlString
