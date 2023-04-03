{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module DBE (
  PostgreSql,
  query,
  query_,
  queryWith,
  queryWith_,
  returning,
  execute,
  execute_,
  executeMany,
  beginMode,
  commit,
  rollback,
  withTransactionMode,
  withTransaction,
  begin,
  runDBE,
  runDBESingle,
  createConnectionPool,
) where

import DBE.Static (createConnectionPool)
import DBE.Static qualified as S
import Data.Pool (Pool)
import Database.PostgreSQL.Simple qualified as SQL
import Database.PostgreSQL.Simple.FromRow qualified as SQL
import Database.PostgreSQL.Simple.Transaction qualified as SQL
import Effectful (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  type (:>),
 )
import Effectful.Dispatch.Dynamic (LocalEnv, localSeqUnlift, reinterpret, send)
import Relude (Int64, ($))

data PostgreSql :: Effect where
  Query :: (SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> PostgreSql m [r]
  Query_ :: (SQL.FromRow r) => SQL.Query -> PostgreSql m [r]
  QueryWith :: (SQL.ToRow q) => SQL.RowParser r -> SQL.Query -> q -> PostgreSql m [r]
  QueryWith_ :: SQL.RowParser r -> SQL.Query -> PostgreSql m [r]
  Returning :: (SQL.ToRow q, SQL.FromRow r) => SQL.Query -> [q] -> PostgreSql m [r]
  Execute :: (SQL.ToRow q) => SQL.Query -> q -> PostgreSql m Int64
  Execute_ :: SQL.Query -> PostgreSql m Int64
  ExecuteMany :: (SQL.ToRow q) => SQL.Query -> [q] -> PostgreSql m Int64
  BeginMode :: SQL.TransactionMode -> PostgreSql m ()
  Commit :: PostgreSql m ()
  Rollback :: PostgreSql m ()
  WithTransactionMode :: SQL.TransactionMode -> m a -> PostgreSql m a

type instance DispatchOf PostgreSql = 'Dynamic

query :: (PostgreSql :> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Eff es [r]
query sql data' = send $ Query sql data'

query_ :: (PostgreSql :> es, SQL.FromRow r) => SQL.Query -> Eff es [r]
query_ sql = send $ Query_ sql

queryWith :: (PostgreSql :> es, SQL.ToRow q) => SQL.RowParser r -> SQL.Query -> q -> Eff es [r]
queryWith rowParser sql data' = send $ QueryWith rowParser sql data'

queryWith_ :: (PostgreSql :> es) => SQL.RowParser r -> SQL.Query -> Eff es [r]
queryWith_ rowParser sql = send $ QueryWith_ rowParser sql

returning :: (PostgreSql :> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> [q] -> Eff es [r]
returning sql data' = send $ Returning sql data'

execute :: (PostgreSql :> es, SQL.ToRow q) => SQL.Query -> q -> Eff es Int64
execute sql data' = send $ Execute sql data'

execute_ :: (PostgreSql :> es) => SQL.Query -> Eff es Int64
execute_ sql = send $ Execute_ sql

executeMany :: (PostgreSql :> es, SQL.ToRow q) => SQL.Query -> [q] -> Eff es Int64
executeMany sql data' = send $ ExecuteMany sql data'

beginMode :: (PostgreSql :> es) => SQL.TransactionMode -> Eff es ()
beginMode transactionMode = send $ BeginMode transactionMode

begin :: (PostgreSql :> es) => Eff es ()
begin = beginMode SQL.defaultTransactionMode

commit :: (PostgreSql :> es) => Eff es ()
commit = send Commit

rollback :: (PostgreSql :> es) => Eff es ()
rollback = send Rollback

withTransactionMode :: (PostgreSql :> es) => SQL.TransactionMode -> Eff es a -> Eff es a
withTransactionMode transactionMode eff = send $ WithTransactionMode transactionMode eff

withTransaction :: (PostgreSql :> es) => Eff es a -> Eff es a
withTransaction = withTransactionMode SQL.defaultTransactionMode

runDBESingle :: (IOE :> es) => SQL.Connection -> Eff (PostgreSql : es) a -> Eff es a
runDBESingle connection = reinterpret (S.runDBESingle connection) localDBE

runDBE :: (IOE :> es) => Pool SQL.Connection -> Eff (PostgreSql : es) a -> Eff es a
runDBE connectionPool = reinterpret (S.runDBE connectionPool) localDBE

localDBE :: (S.PostgreSql :> es) => LocalEnv localEs es -> PostgreSql (Eff localEs) a -> Eff es a
localDBE env = \case
  Query sql data' -> S.query sql data'
  Query_ sql -> S.query_ sql
  QueryWith rowParser sql data' -> S.queryWith rowParser sql data'
  QueryWith_ rowParser sql -> S.queryWith_ rowParser sql
  Returning sql data' -> S.returning sql data'
  Execute sql data' -> S.execute sql data'
  Execute_ sql -> S.execute_ sql
  ExecuteMany sql data' -> S.executeMany sql data'
  BeginMode transactionMode -> S.beginMode transactionMode
  Commit -> S.commit
  Rollback -> S.rollback
  WithTransactionMode transactionMode eff -> localSeqUnlift env $ \unlift -> S.withTransactionMode transactionMode (unlift eff)
