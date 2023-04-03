{-# LANGUAGE TypeFamilies #-}

module DBE.Static (
  PostgreSql,
  query,
  query_,
  queryWith,
  queryWith_,
  beginMode,
  executeMany,
  execute_,
  execute,
  returning,
  begin,
  commit,
  rollback,
  withTransactionMode,
  withTransaction,
  createConnectionPool,
  runDBE,
  runDBESingle,
) where

import Control.Monad.Catch (
  MonadCatch (catch),
  MonadMask (mask),
  bracket,
  onException,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString
import Data.Int (Int64)
import Data.Pool (
  Pool,
  defaultPoolConfig,
  newPool,
  putResource,
  takeResource,
 )
import Database.PostgreSQL.Simple qualified as SQL
import Database.PostgreSQL.Simple.FromRow qualified as SQL
import Database.PostgreSQL.Simple.Transaction qualified as SQL
import Effectful (
  Dispatch (Static),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  type (:>),
 )
import Effectful.Dispatch.Static (
  SideEffects (WithSideEffects),
  StaticRep,
  evalStaticRep,
  getStaticRep,
  unsafeEff_,
 )
import Prelude (IOError, pure, ($))

data PostgreSql :: Effect

type instance DispatchOf PostgreSql = 'Static 'WithSideEffects

newtype instance StaticRep PostgreSql = PostgreSql' {connection :: SQL.Connection}

query :: (PostgreSql :> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Eff es [r]
query sql data' = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.query connection sql data'

query_ :: (PostgreSql :> es, SQL.FromRow r) => SQL.Query -> Eff es [r]
query_ sql = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.query_ connection sql

queryWith :: (PostgreSql :> es, SQL.ToRow q) => SQL.RowParser r -> SQL.Query -> q -> Eff es [r]
queryWith rowParser sql data' = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.queryWith rowParser connection sql data'

queryWith_ :: (PostgreSql :> es) => SQL.RowParser r -> SQL.Query -> Eff es [r]
queryWith_ rowParser sql = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.queryWith_ rowParser connection sql

returning :: (PostgreSql :> es, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> [q] -> Eff es [r]
returning sql data' = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.returning connection sql data'

execute :: (PostgreSql :> es, SQL.ToRow q) => SQL.Query -> q -> Eff es Int64
execute sql data' = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.execute connection sql data'

execute_ :: (PostgreSql :> es) => SQL.Query -> Eff es Int64
execute_ sql = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.execute_ connection sql

executeMany :: (PostgreSql :> es, SQL.ToRow q) => SQL.Query -> [q] -> Eff es Int64
executeMany sql data' = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.executeMany connection sql data'

beginMode :: (PostgreSql :> es) => SQL.TransactionMode -> Eff es ()
beginMode transactionMode = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.beginMode transactionMode connection

begin :: (PostgreSql :> es) => Eff es ()
begin = beginMode SQL.defaultTransactionMode

commit :: (PostgreSql :> es) => Eff es ()
commit = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.commit connection

rollback :: (PostgreSql :> es) => Eff es ()
rollback = do
  PostgreSql' {..} <- getStaticRep
  unsafeEff_ $ SQL.rollback connection

rollback_ :: (PostgreSql :> es) => Eff es ()
rollback_ = rollback `catch` \(_ :: IOError) -> pure ()

withTransactionMode :: (PostgreSql :> es) => SQL.TransactionMode -> Eff es a -> Eff es a
withTransactionMode transactionMode eff = do
  mask $ \restore -> do
    beginMode transactionMode
    r <- restore eff `onException` rollback_
    commit
    pure r

withTransaction :: (PostgreSql :> es) => Eff es a -> Eff es a
withTransaction = withTransactionMode SQL.defaultTransactionMode

createConnectionPool :: (MonadIO m) => ByteString -> m (Pool SQL.Connection)
createConnectionPool connectionInfo = liftIO $ newPool poolConfig
  where
    poolConfig =
      defaultPoolConfig (SQL.connectPostgreSQL connectionInfo) SQL.close 60 60

runDBESingle :: (IOE :> es) => SQL.Connection -> Eff (PostgreSql : es) a -> Eff es a
runDBESingle connection = evalStaticRep (PostgreSql' connection)

runDBE :: (IOE :> es) => Pool SQL.Connection -> Eff (PostgreSql : es) a -> Eff es a
runDBE connectionPool eff = do
  bracket
    (liftIO $ takeResource connectionPool)
    (\(connection, localPoll) -> liftIO $ putResource localPoll connection)
    (\(connection, _) -> evalStaticRep (PostgreSql' connection) eff)
