{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DataModel.Service
    ( DataModel
    , addUser
    , createDataModelChannel
    , addEpisodeEntryifUnique
    , listAllFeeds
    , runDataModel
    , runDataModelEffect
    , listUsers
    , deleteUser
    , selectUser
    , selectUserPassword
    )
  where

import Conduit (ResourceT)
import Control.Applicative ((<$>), pure)
import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad ((>>), (>>=), void)
import Control.Monad.Extra (whenJust)
import Database.Persist
    ( Entity(Entity, entityVal)
    , (==.)
    , insertUniqueEntity
    , upsertBy
    , selectList
    , selectFirst
    , delete
    )
import Database.Persist.Sqlite (runSqlite, )
import Database.Persist.Sql (SqlBackend, runMigration, fromSqlKey, toSqlKey, transactionSave)
import Data.Function (($), (.))
import Data.Functor (Functor(fmap))
import Data.Maybe (listToMaybe, Maybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import System.IO (IO)

import Core.Type.EpisodeEntry (EpisodeEntry(EpisodeEntry, title, url, imageUrl, episodeNumber, animeUrl))
import Core.Type.Id (fromId, UserId, unsafeId)
import qualified Core.Type.Episode as Core
    (Episode (Episode, date, episodeId, title, imageUrl, date, url, number))
import qualified Core.Type.User as Core
    ( NewUser(NewUser, name, email, password)
    , User(User, userId, name, email, password, episodeChannel), Email
    )
import Control.Monad.Freer.Service
    ( IscCall(ChannelData, get, put)
    , ServiceChannel
    , createServiceChannel
    , runServiceChannel
    , runServiceEffect
    )
import DataModel.Persistent
    (  animeDate
    , Episode(Episode, episodeUrl, episodeNumber, episodeDate, episodeAnimeId)
    , Anime(Anime, animeTitle, animeImgUrl, animeAnimeUrl)
    , User(User, userName, userEmail, userPassword, userNewEpisodeChannel)
    , EntityField(EpisodeDate, EpisodeAnimeId, UserEmail, UserId, AnimeId)
    , UniqueAnimeTitle
    , migrateAll
    )
import Database.Esqueleto ((^.), select, from, orderBy, desc, where_)
import qualified Database.Esqueleto as Esql ((==.))

import Crypto.PasswordStore (PasswordHash)


data DataModel s where
    AddUser :: Core.NewUser -> DataModel (Maybe Core.User)
    ListUsers :: DataModel ([Core.User])
    DeleteUser :: UserId -> DataModel ()
    SelectUserPassword :: Core.Email -> DataModel (Maybe PasswordHash)
    SelectUser :: UserId -> DataModel (Maybe Core.User)
    AddEpisodeEntryIfUnique :: EpisodeEntry -> DataModel ()
    ListAllFeeds :: DataModel ([Core.Episode], Maybe UTCTime)

addEpisodeEntryifUnique :: Member DataModel effs => EpisodeEntry -> Eff effs ()
addEpisodeEntryifUnique = send . AddEpisodeEntryIfUnique

listAllFeeds :: Member DataModel effs => Eff effs ([Core.Episode], Maybe UTCTime)
listAllFeeds = send ListAllFeeds

addUser :: Member DataModel effs => Core.NewUser -> Eff effs (Maybe Core.User)
addUser = send . AddUser

listUsers :: Member DataModel effs => Eff effs [Core.User]
listUsers = send ListUsers

deleteUser :: Member DataModel effs => UserId -> Eff effs ()
deleteUser = send . DeleteUser

selectUserPassword
    :: Member DataModel effs => Core.Email -> Eff effs (Maybe PasswordHash)
selectUserPassword = send . SelectUserPassword

selectUser :: Member DataModel effs => UserId -> Eff effs (Maybe Core.User)
selectUser = send . SelectUser

instance IscCall DataModel where
    data ChannelData DataModel a = WrapDataModel (DataModel a)

    get :: ChannelData DataModel a -> DataModel a
    get (WrapDataModel v) = v

    put :: DataModel a -> ChannelData DataModel a
    put = WrapDataModel

type DataModelMonad = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

processDataModel :: forall a. (a -> DataModelMonad ()) -> DataModel a -> DataModelMonad ()
processDataModel return' action = f action >> transactionSave
  where
    f :: DataModel a -> DataModelMonad ()
    f = \case
        AddUser user -> do
            user' <- createDbUser user >>= insertUniqueEntity
            return' $ fmap toCoreUser user'
        AddEpisodeEntryIfUnique animeEntry -> addEpisode animeEntry >>= return'
        ListAllFeeds -> do
            feeds <- (fmap toCoreEpisode) <$> selectLastEpisodes
            return' (feeds, Core.date <$> listToMaybe feeds)
        ListUsers -> (fmap toCoreUser) <$> selectList [] [] >>= return'
        DeleteUser userId -> do
            delete . toSqlKey @User $ fromId userId
            return' ()
        SelectUserPassword email -> do
            password <- selectFirst [UserEmail ==. email] []
            return' $ fmap (userPassword . entityVal) password
        SelectUser userId -> do
            user <- selectFirst [UserId ==. (toSqlKey @User $ fromId userId)] []
            return' $ fmap toCoreUser user

selectLastEpisodes :: DataModelMonad [(Entity Episode, Entity Anime)]
selectLastEpisodes = do
    select $ from $ \(anime, episode) -> do
        where_ (anime ^. AnimeId Esql.==. episode ^. EpisodeAnimeId)
        orderBy [desc $ episode ^. EpisodeDate]
        pure (episode, anime)

toCoreEpisode :: (Entity Episode, Entity Anime) -> Core.Episode
toCoreEpisode (Entity key Episode{..}, Entity _ Anime{..}) = Core.Episode
    { episodeId = unsafeId $ fromSqlKey key
    , title = animeTitle
    , imageUrl = animeImgUrl
    , date = episodeDate
    , url = episodeUrl
    , number = episodeNumber
    }

toCoreUser :: Entity User -> Core.User
toCoreUser (Entity key User{..}) = Core.User
    { userId = unsafeId $ fromSqlKey key
    , name = userName
    , email = userEmail
    , password = userPassword
    , episodeChannel = userNewEpisodeChannel
    }

createDbUser :: MonadIO m => Core.NewUser -> m User
createDbUser Core.NewUser{..} = do
    episodeChannel <- liftIO nextRandom
    pure $ User
        { userName = name
        , userEmail = email
        , userPassword = password
        , userNewEpisodeChannel = episodeChannel
        }

addEpisode :: EpisodeEntry -> DataModelMonad ()
addEpisode EpisodeEntry{..} = do
    currentTime <- liftIO $ getCurrentTime
    let anime = Anime
            { animeTitle = title
            , animeImgUrl = imageUrl
            , animeAnimeUrl = animeUrl
            , animeDate = currentTime
            }
    mAnimeId <- upsertBy (UniqueAnimeTitle title) anime []
    whenJust mAnimeId $ \(Entity key _) -> void . insertUniqueEntity $ Episode
        { episodeAnimeId = key
        , episodeUrl = url
        , episodeNumber = episodeNumber
        , episodeDate = currentTime
        }

createDataModelChannel :: IO (ServiceChannel DataModel)
createDataModelChannel = createServiceChannel

-- TODO: Get the connection string as argument.
runDataModel :: ServiceChannel DataModel -> IO ()
runDataModel chan = runSqlite "animerssdb.sqlite3" $ do
    runMigration migrateAll
    transactionSave
    runServiceChannel chan processDataModel

runDataModelEffect
    :: Member IO effs => ServiceChannel DataModel
    -> Eff (DataModel ': effs) a
    -> Eff effs a
runDataModelEffect = runServiceEffect
