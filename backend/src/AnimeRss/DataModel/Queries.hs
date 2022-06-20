{-# LANGUAGE QuasiQuotes #-}

module AnimeRss.DataModel.Queries
  ( insertDbUser
  , getDbUserById
  , listDbUsers
  , deleteDbUser
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

insertDbUser :: ([PostgreSql, IOE] :>> es) => CreateUser -> Eff es (Maybe GetUser)
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

getDbUserById :: ([PostgreSql, IOE] :>> es) => UserId -> Eff es (Maybe GetUser)
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

listDbUsers :: ([PostgreSql, IOE] :>> es) => Eff es [GetUser]
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

deleteDbUser :: ([PostgreSql, IOE] :>> es) => UserId -> Eff es ()
deleteDbUser userId = do
  ret <- execute
    [sql| DELETE FROM users WHERE id = ?
    |]
    (SQL.Only userId)
  expectOneAction "deleteDBUsers" ret
