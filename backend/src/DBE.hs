{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module DBE
  ( PostgreSql
  , createConnectionPool
  , runDBE
  , query
  , query_
  , queryWith
  , queryWith_
  , returning
  , execute
  , execute_
  , executeMany
  , beginMode
  , commit
  , rollback
  , withTransactionMode
  , withTransaction
  , begin
  , runDBESingle
  )
where

import Control.Monad.Catch
import Data.Pool
import qualified Database.PostgreSQL.Simple as SQL
import qualified Database.PostgreSQL.Simple.FromRow as SQL
import qualified Database.PostgreSQL.Simple.Transaction as SQL
import Effectful
import Effectful.Reader.Static
import Relude hiding (Reader, ask, id, runReader)

query :: ([PostgreSql, IOE] :>> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Eff es [r]
query sql data' = do
  connection <- ask
  liftIO $ SQL.query connection sql data'

query_ :: ([PostgreSql, IOE] :>> es, SQL.FromRow r) => SQL.Query -> Eff es [r]
query_ sql = do
  connection <- ask
  liftIO $ SQL.query_ connection sql

queryWith :: ([PostgreSql, IOE] :>> es, SQL.ToRow q) => SQL.RowParser r -> SQL.Query -> q -> Eff es [r]
queryWith rowParser sql data' = do
  connection <- ask
  liftIO $ SQL.queryWith rowParser connection sql data'

queryWith_ :: ([PostgreSql, IOE] :>> es) => SQL.RowParser r -> SQL.Query -> Eff es [r]
queryWith_ rowParser sql = do
  connection <- ask
  liftIO $ SQL.queryWith_ rowParser connection sql

returning :: ([PostgreSql, IOE] :>> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> [q] -> Eff es [r]
returning sql data' = do
  connection <- ask
  liftIO $ SQL.returning connection sql data'

execute :: ([PostgreSql, IOE] :>> es, SQL.ToRow q) => SQL.Query -> q -> Eff es Int64
execute sql data' = do
  connection <- ask
  liftIO $ SQL.execute connection sql data'

execute_ :: ([PostgreSql, IOE] :>> es) => SQL.Query -> Eff es Int64
execute_ sql = do
  connection <- ask
  liftIO $ SQL.execute_ connection sql

executeMany :: ([PostgreSql, IOE] :>> es, SQL.ToRow q) => SQL.Query -> [q] -> Eff es Int64
executeMany sql data' = do
  connection <- ask
  liftIO $ SQL.executeMany connection sql data'

beginMode :: ([PostgreSql, IOE] :>> es) => SQL.TransactionMode -> Eff es ()
beginMode transactionMode = do
  connection <- ask
  liftIO $ SQL.beginMode transactionMode connection

begin :: ([PostgreSql, IOE] :>> es) => Eff es ()
begin = beginMode SQL.defaultTransactionMode

commit :: ([PostgreSql, IOE] :>> es) => Eff es ()
commit = do
  connection <- ask
  liftIO $ SQL.commit connection

rollback :: ([PostgreSql, IOE] :>> es) => Eff es ()
rollback = do
  connection <- ask
  liftIO $ SQL.rollback connection

withTransactionMode :: ([PostgreSql, IOE] :>> es) => SQL.TransactionMode -> Eff es a -> Eff es a
withTransactionMode transactionMode act = do
  mask $ \restore -> do
    beginMode transactionMode
    r <- restore act `onException` rollback
    commit
    return r

withTransaction :: ([PostgreSql, IOE] :>> es) => Eff es a -> Eff es a
withTransaction = withTransactionMode SQL.defaultTransactionMode

type PostgreSql = Reader SQL.Connection

type DBConnectionPool = Pool SQL.Connection

createConnectionPool :: SQL.ConnectInfo -> IO DBConnectionPool
createConnectionPool connectionInfo = newPool poolConfig
  where
    poolConfig =
      PoolConfig
        { createResource = SQL.connect connectionInfo
        , freeResource = SQL.close
        , poolCacheTTL = 60
        , poolMaxResources = 60
        }

-- | Run the 'FileSystem' effect.
runDBE :: (IOE :> es) => DBConnectionPool -> Eff (PostgreSql : es) a -> Eff es a
runDBE connectionPool eff = do
  bracket
    (liftIO $ takeResource connectionPool)
    (\(connection, localPoll) -> liftIO $ putResource localPoll connection)
    (\(connection, _) -> runReader connection eff)

runDBESingle :: SQL.Connection -> Eff (PostgreSql : es) a -> Eff es a
runDBESingle connection eff = runReader connection eff

