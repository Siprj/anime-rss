{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AnimeRss.Rest.Server (
  apiHander,
  authHandlerSession,
  AuthSessionHandler,
) where

import AnimeRss.DataModel.Queries (deleteUserFollows, getDbUserById, insertUserFollows, insertUserSession, listAnimes, listEpisodesByChannelId, listUserRelatedAnime, selectUserByEmail, selectUserBySession, selectGogoAnimeUrl)
import AnimeRss.DataModel.Types (CreateUserFollows (..), DeleteUserFollows (..), toPasswordHash)
import AnimeRss.DataModel.Types qualified as Core
import AnimeRss.Ids (fromId)
import AnimeRss.Rest.Api (Anime (..), Api, ChannelId, LoggedInUser (LoggedInUser, userId), Login (..), PostAnimeFollow (..), Protected, SubParam (..), User (..))
import AnimeRss.Rest.Auth
import AnimeRss.Rest.Authentication ()
import Control.Applicative (pure)
import Control.Monad (Monad ((>>)), (>>=))
import Crypto.Error (CryptoFailable (..))
import Crypto.PasswordStore
import DBE (PostgreSql)
import Data.Bool (Bool (True))
import Data.ByteString hiding (pack, unpack)
import Data.CaseInsensitive qualified as CI
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.List
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Time (
  UTCTime,
  defaultTimeLocale,
  formatTime,
  rfc822DateFormat,
 )
import Data.UUID (fromASCIIBytes, toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Effectful (Eff, IOE, MonadIO (..), (:>))
import Effectful.Error.Dynamic (Error, throwError)
import Network.HTTP.Types (hCookie)
import Network.Wai
import Otel.Effect
import Otel.Type
import Relude (show, unless, (==))
import Servant (Header, Headers, NoContent (NoContent), ServerError, ServerT, URI, addHeader, err401, err409, err500, (:<|>) ((:<|>)))
import Servant.Server (Handler)
import Text.Atom.Feed (
  Entry (entryLinks, entrySummary),
  TextContent (HTMLString, TextString),
  feedEntries,
  feedLinks,
  nullEntry,
  nullFeed,
  nullLink,
 )
import Text.Atom.Feed qualified as Atom (Feed)
import Text.Feed.Constructor (feedFromAtom)
import Text.Feed.Types (Feed)
import Web.Cookie

type instance APIAuthServerData (APIAuthProtect "session") = LoggedInUser

type instance APIAuthServerDataCookies (APIAuthProtect "session") = ZeroCookies

type AuthSessionHandler = APIAuthHandler Request LoggedInUser ZeroCookies

xTokenName :: ByteString
xTokenName = "x-token"

sessionName :: ByteString
sessionName = "session"

authHandlerSession :: (Error ServerError :> es, PostgreSql :> es, Otel :> es) => (Eff es (LoggedInUser, SetCookieList ZeroCookies) -> Handler (LoggedInUser, SetCookieList ZeroCookies)) -> AuthSessionHandler
authHandlerSession runEffectStack = mkAPIAuthHandler handler
  where
    handler :: Request -> Handler (LoggedInUser, SetCookieList ZeroCookies)
    handler req = runEffectStack . traceInternal_ "authHandlerSession" $ do
      cookies' <- justOrErr401 "Cookie header is missing." . lookup hCookie $ requestHeaders req
      let cookies = parseCookies cookies'
      sessionIdBs <- justOrErr401 "Session cookie is missing." $ lookup sessionName cookies
      xToken <- justOrErr401 "x-token cookie is missing." $ lookup xTokenName cookies
      xTokenHeader <- justOrErr401 "x-token header is missing." . lookup (CI.mk xTokenName) $ requestHeaders req
      unless (xToken == xTokenHeader) $ do
        let attributes =
              [ KeyValue "xtoken-cookie" . StringV $ show xToken
              , KeyValue "xtoken-cookie" . StringV $ show xTokenHeader
              ]
        logInfo attributes "xtokens are not the same"
        throwError err401
      sessionId <- justOrErr401 "Session ID is not valid UUID" $ fromASCIIBytes sessionIdBs
      userId <- selectUserBySession sessionId >>= justOrErr401 "Session ID not found"
      pure (LoggedInUser {..}, SetCookieNil)

    justOrErr401 :: (Otel :> es, Error ServerError :> es) => Text -> Maybe a -> Eff es a
    justOrErr401 _ (Just v) = pure v
    -- TODO: Add logging for errors...
    justOrErr401 logMessage Nothing = logInfo_ logMessage >> throwError err401

type Handler' es = (Error ServerError :> es, PostgreSql :> es)

type RestServer api es = ServerT api (Eff es)

maybeThrow :: Handler' es => ServerError -> Maybe a -> Eff es a
maybeThrow e = \case
  Nothing -> throwError e
  Just v -> pure v

protectedHandlers :: (Handler' es, Otel :> es) => LoggedInUser -> RestServer Protected es
protectedHandlers user = go
  where
    go =
      userGetHandler user
        :<|> animeUpdateHandler user
        :<|> animeListHandler user

userGetHandler :: (Handler' es, Otel :> es) => LoggedInUser -> Eff es User
userGetHandler loggedInUser = traceInternal_ "userGetHandler" $ do
  getDbUserById loggedInUser.userId >>= maybeThrow err409 . fmap toUser
  where
    toUser Core.User {..} =
      User
        { userId = id
        , email
        , name
        , episodeChannel = newsChannel
        }

animeUpdateHandler :: (Handler' es, Otel :> es) => LoggedInUser -> PostAnimeFollow -> Eff es NoContent
animeUpdateHandler loggedInUser follow = traceInternal_ "animeUpdateHandler" $ do
  if follow.follow
    then insertUserFollows $ CreateUserFollows loggedInUser.userId follow.animeId
    else deleteUserFollows $ DeleteUserFollows loggedInUser.userId follow.animeId
  pure NoContent

animeListHandler :: (Handler' es, Otel :> es) => LoggedInUser -> Maybe SubParam -> Maybe Text -> Eff es [Anime]
animeListHandler loggedInUser mSubParam mSearch = traceInternal_ "animeListHandler" $ do
  animeList <- listUserRelatedAnime loggedInUser.userId (fromMaybe All mSubParam) mSearch
  pure $ fmap toApiAnime animeList
  where
    toApiAnime Core.UserRelatedAnime {..} =
      Anime
        { animeId
        , title
        , url = url
        , imageUrl = imageUrl
        , date
        , following
        }

loginHandler :: (PostgreSql :> es, Error ServerError :> es, Otel :> es, IOE :> es) => Login -> Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler Login {..} = traceInternal_ "loginHandler" $ do
  user <- selectUserByEmail email >>= maybe (throwError err401) pure
  logInfo_ $ "selected user: " <> show user
  case verifyPassword (encodeUtf8 password) . toPasswordHash $ user.password of
    CryptoPassed v ->
      if v
        then do
          logInfo_ $ "User authenticated: " <> show user
        else do
          logInfo_ $ "User authentication failed: " <> show user
          throwError err401
    CryptoFailed e -> do
      logError_ $ show e
      throwError err401
  sessionId <- insertUserSession user.id >>= maybe (throwError err500) pure
  let sessionCookie = defaultSetCookie {setCookieName = sessionName, setCookieValue = encodeUtf8 . fromId $ sessionId, setCookieHttpOnly = True, setCookiePath = Just "/"}
  xtokenUUID <- liftIO nextRandom
  let xtokenCookie = defaultSetCookie {setCookieName = xTokenName, setCookieValue = toASCIIBytes xtokenUUID, setCookiePath = Just "/"}
  pure . addHeader sessionCookie . addHeader xtokenCookie $ NoContent

atomEpisodesGetHandler :: (Handler' es, Otel :> es) => ChannelId -> Eff es Feed
atomEpisodesGetHandler channelId = traceInternal_ "atomEpisodesGetHandler" $ do
  episodes <- listEpisodesByChannelId channelId
  baseUri <- selectGogoAnimeUrl

  let lastModification = Nothing

  pure . feedFromAtom . fd baseUri lastModification $
    fmap toEntry episodes
  where
    fd :: URI -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd baseUri date entries =
      fd'
        { feedLinks = [nullLink $ show baseUri]
        , feedEntries = entries
        }
      where
        fd' =
          nullFeed (show baseUri) (TextString "episode channel") . pack $
            maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Episode -> Entry
    toEntry Core.Episode {..} =
      entry'
        { entryLinks = [nullLink $ show url]
        , entrySummary =
            Just . HTMLString $
              "<div><a href=\""
                <> show url
                <> "\"><img src=\""
                <> show imageUrl
                <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' =
          nullEntry
            (show url)
            (TextString $ title <> " [Episode: " <> number <> "]")
            . pack
            $ formatTime defaultTimeLocale rfc822DateFormat date

atomAnimeGetHandler :: (Otel :> es, Handler' es) => Eff es Feed
atomAnimeGetHandler = traceInternal_ "atomAnimeGetHandler" $ do
  animes <- listAnimes
  baseUri <- selectGogoAnimeUrl

  let lastModification = Nothing

  pure . feedFromAtom . fd baseUri lastModification $
    fmap toEntry animes
  where
    fd :: URI -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd baseUri date entries =
      fd'
        { feedLinks = [nullLink $ show baseUri]
        , feedEntries = entries
        }
      where
        fd' =
          nullFeed (show baseUri) (TextString "anime channel") . pack $
            maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Anime -> Entry
    toEntry Core.Anime {..} =
      entry'
        { entryLinks = [nullLink $ show url]
        , entrySummary =
            Just . HTMLString
            -- TODO: Add link to the server with possibility to follow the anime
            $
              "<div><a href=\""
                <> show url
                <> "\"><img src=\""
                <> show imgUrl
                <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' =
          nullEntry
            (show url)
            (TextString title)
            . pack
            $ formatTime defaultTimeLocale rfc822DateFormat date

apiHander :: (Handler' es, Otel :> es, IOE :> es) => RestServer Api es
apiHander =
  atomEpisodesGetHandler
    :<|> atomAnimeGetHandler
    :<|> loginHandler
    :<|> protectedHandlers
