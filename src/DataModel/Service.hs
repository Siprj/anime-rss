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
    , runDataModel
    , runDataModelEffect
    , listUsers
    , deleteUser
    , selectUser
    , selectUserPassword
    , modifyUsersAnime
    , listAnime
    , listUserRelatedAnime
    , listChannelEpisodes
    )
  where

import Conduit (ResourceT)
import Control.Applicative ((<$>), pure)
import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad ((>>), (>>=), void)
import Database.Persist
    ( Entity(Entity, entityVal, entityKey)
    , (==.)
    , (=.)
    , insertUniqueEntity
    , upsert
    , selectList
    , selectFirst
    , delete
    , insertBy, SelectOpt (Desc)
    )
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.Sql (SqlBackend, runMigration, fromSqlKey, toSqlKey, transactionSave)
import Data.Function (($), (.), id)
import Data.Functor (Functor(fmap))
import Data.Maybe (Maybe)
import Data.Time.Clock (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import System.IO (IO)

import Core.Type.EpisodeEntry (EpisodeEntry(EpisodeEntry, title, url, imageUrl, number, animeUrl))
import Core.Type.Id (fromId, UserId, unsafeId)
import qualified Core.Type.User as Core
    ( NewUser(NewUser, name, email, password)
    , User(User, userId, name, email, password, episodeChannel), Email
    )
import qualified Core.Type.Episode as Core
    ( Episode(Episode, title, url, imageUrl, date, number))
import qualified Core.Type.UserFollow as Core
    ( UserFollow(UserFollow, userId , animeId , follow)
    )
import qualified Core.Type.Anime as Core
    ( Anime(Anime, animeId, title, url, imageUrl, date)
    , UserRelatedAnime(UserRelatedAnime, animeId, title, url, imageUrl, date, following)
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
    , Anime(Anime, animeTitle, animeImgUrl, animeUrl)
    , User(User, userName, userEmail, userPassword, userNewEpisodeChannel)
    , EntityField(UserEmail, UserId, AnimeId, UserFollowFollow, AnimeDate, UserFollowUserId, UserFollowAnimeId, EpisodeDate, EpisodeAnimeId, UserNewEpisodeChannel)

    , UserFollow(UserFollow, userFollowUserId, userFollowAnimeId, userFollowFollow)
    , migrateAll
    )
import Database.Esqueleto ((^.), select, from, orderBy, desc, where_, LeftOuterJoin(LeftOuterJoin), on, (?.), just, val, Value(Value), coalesceDefault, InnerJoin(InnerJoin), limit)
import qualified Database.Esqueleto as Esql ((==.))

import Crypto.PasswordStore (PasswordHash)
import Data.Either (either)
import Data.Bool (Bool (False))
import Data.Tuple (uncurry)
import Data.UUID (UUID)


data DataModel s where
    -- User manipulation
    AddUser :: Core.NewUser -> DataModel (Maybe Core.User)
    DeleteUser :: UserId -> DataModel ()
    ListUsers :: DataModel [Core.User]
    SelectUserPassword :: Core.Email -> DataModel (Maybe PasswordHash)
    SelectUser :: UserId -> DataModel (Maybe Core.User)

    -- Anime manipulation
    ModifyUsersAnime :: Core.UserFollow -> DataModel (Maybe Core.UserRelatedAnime)
    AddEpisodeEntryIfUnique :: EpisodeEntry -> DataModel ()
    ListAnime :: DataModel [Core.Anime]
    ListUserRelatedAnime :: UserId -> DataModel [Core.UserRelatedAnime]
    ListChannelEpisodes :: UUID -> DataModel [Core.Episode]

addEpisodeEntryifUnique :: Member DataModel effs => EpisodeEntry -> Eff effs ()
addEpisodeEntryifUnique = send . AddEpisodeEntryIfUnique

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

modifyUsersAnime :: Member DataModel effs => Core.UserFollow -> Eff effs (Maybe Core.UserRelatedAnime)
modifyUsersAnime = send . ModifyUsersAnime

listAnime :: Member DataModel effs => Eff effs [Core.Anime]
listAnime = send ListAnime

listUserRelatedAnime :: Member DataModel effs => UserId -> Eff effs [Core.UserRelatedAnime]
listUserRelatedAnime = send . ListUserRelatedAnime

listChannelEpisodes :: Member DataModel effs => UUID -> Eff effs [Core.Episode]
listChannelEpisodes = send . ListChannelEpisodes

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
        -- User Manipulation
        AddUser user -> do
            user' <- createDbUser user >>= insertUniqueEntity
            return' $ fmap toCoreUser user'
        DeleteUser userId -> do
            delete . toSqlKey @User $ fromId userId
            return' ()
        ListUsers -> selectList [] [] >>= return' . fmap toCoreUser
        SelectUserPassword email -> do
            password <- selectFirst [UserEmail ==. email] []
            return' $ fmap (userPassword . entityVal) password
        SelectUser userId -> do
            user <- selectFirst [UserId ==. (toSqlKey @User $ fromId userId)] []
            return' $ fmap toCoreUser user

        -- Anime manipulation
        AddEpisodeEntryIfUnique animeEntry -> addEpisode animeEntry >>= return'
        ModifyUsersAnime userFollow@Core.UserFollow{..} -> do
            void $ upsert (fromUserFollow userFollow) [UserFollowFollow =. follow]
            anime <- selectFirst [AnimeId ==. (toSqlKey @Anime $ fromId animeId)] []
            return' $ fmap (toCoreUserRelatedAnime (Value follow)) anime
        ListAnime -> selectAnimes >>= return' . fmap toCoreAnime
        ListUserRelatedAnime userId -> do
            selectUserRelatedAnimes userId >>= return' . fmap (uncurry toCoreUserRelatedAnime)
        ListChannelEpisodes channelId -> do
            selectEpisodesByChannelId channelId >>= return' . fmap toCoreEpisode



selectAnimes :: DataModelMonad [Entity Anime]
selectAnimes = selectList [] [Desc AnimeDate]

-- TODO: Chekc this works as expected.
--   This part looks broken: just (val (toSqlKey @User $ fromId userId)))
selectUserRelatedAnimes :: UserId -> DataModelMonad [(Value Bool, Entity Anime)]
selectUserRelatedAnimes userId = do
    select . from $ \(anime `LeftOuterJoin` userFollow) -> do
        on (just (anime ^. AnimeId) Esql.==. userFollow ?. UserFollowAnimeId)
        orderBy [desc $ anime ^. AnimeDate]
        where_ (userFollow ?. UserFollowUserId Esql.==. just (val (toSqlKey @User $ fromId userId)))
        pure (coalesceDefault [userFollow ?. UserFollowFollow] (val False), anime)

selectEpisodesByChannelId :: UUID -> DataModelMonad [(Entity Episode, Entity Anime)]
selectEpisodesByChannelId channelId = do
    select . from $ \(episode `InnerJoin` userFollow `InnerJoin` user `InnerJoin` anime) -> do
        orderBy [desc $ episode ^. EpisodeDate]
        limit 500
        on ((episode ^. EpisodeAnimeId) Esql.==. userFollow ^. UserFollowAnimeId)
        on ((userFollow ^. UserFollowUserId) Esql.==. user ^. UserId)
        on ((userFollow ^. UserFollowAnimeId) Esql.==. anime ^. AnimeId)
        where_ (user ^. UserNewEpisodeChannel Esql.==. val channelId)
        pure (episode, anime)

toCoreEpisode :: (Entity Episode, Entity Anime) -> Core.Episode
toCoreEpisode (Entity _ Episode{..}, Entity _ Anime{..}) = Core.Episode
    { title = animeTitle
    , imageUrl = animeImgUrl
    , date = episodeDate
    , url = episodeUrl
    , number = episodeNumber
    }

fromUserFollow :: Core.UserFollow -> UserFollow
fromUserFollow Core.UserFollow{..} = UserFollow
    { userFollowUserId = toSqlKey @User $ fromId userId
    , userFollowAnimeId = toSqlKey @Anime $ fromId animeId
    , userFollowFollow = follow
    }

toCoreUser :: Entity User -> Core.User
toCoreUser (Entity key User{..}) = Core.User
    { userId = unsafeId $ fromSqlKey key
    , name = userName
    , email = userEmail
    , password = userPassword
    , episodeChannel = userNewEpisodeChannel
    }

toCoreAnime :: Entity Anime -> Core.Anime
toCoreAnime (Entity key Anime{..}) = Core.Anime
    { animeId = unsafeId $ fromSqlKey key
    , title = animeTitle
    , url = animeUrl
    , imageUrl = animeImgUrl
    , date = animeDate
    }

toCoreUserRelatedAnime :: Value Bool -> Entity Anime -> Core.UserRelatedAnime
toCoreUserRelatedAnime (Value following) (Entity key Anime{..}) = Core.UserRelatedAnime
    { animeId = unsafeId $ fromSqlKey key
    , title = animeTitle
    , url = animeUrl
    , imageUrl = animeImgUrl
    , date = animeDate
    , following = following
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
    currentTime <- liftIO getCurrentTime
    let anime = Anime
            { animeTitle = title
            , animeImgUrl = imageUrl
            , animeUrl = animeUrl
            , animeDate = currentTime
            }
    animeId <- either entityKey id <$> insertBy anime
    void . insertUniqueEntity $ Episode
        { episodeAnimeId = animeId
        , episodeUrl = url
        , episodeNumber = number
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
