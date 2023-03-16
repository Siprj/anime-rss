{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Rest.AtomMime
    (AtomFeed)
  where

import Control.Exception (Exception, throw)
import Data.ByteString.Lazy (ByteString)
import Data.Default(def)
import Data.Either (Either(Right, Left))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy(Proxy)
import Data.Set (Set)
import Data.String (String)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text)
import Servant.API.ContentTypes
    ( Accept(contentType)
    , MimeRender(mimeRender)
    , MimeUnrender(mimeUnrender)
    )
import Network.HTTP.Media ((//))
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Text.Show (Show)
import Text.XML
    ( Document(Document, documentEpilogue, documentPrologue, documentRoot)
    , Prologue(Prologue, prologueAfter, prologueBefore, prologueDoctype)
    , fromXMLElement
    , renderText
    )


data AtomFeed

instance Accept AtomFeed where
    contentType _ = "application" // "atom+xml"

newtype FeedRendererException = FeedRendererException (Set Text)
    deriving stock Show

instance Exception FeedRendererException

instance MimeRender AtomFeed Feed where
    mimeRender :: Proxy AtomFeed -> Feed -> ByteString
    mimeRender _ val =
        encodeUtf8 . renderText def . toDocument . fromRight' . fromXMLElement
            $ xmlFeed val
      where
        toDocument v = Document
            { documentPrologue = Prologue
                { prologueBefore = []
                , prologueDoctype = Nothing
                , prologueAfter = []
                }
            , documentRoot = v
            , documentEpilogue = []
            }

        fromRight' (Right v) = v
        fromRight' (Left e) = throw $ FeedRendererException e

instance MimeUnrender AtomFeed Feed where
    mimeUnrender :: Proxy AtomFeed -> ByteString -> Either String Feed
    mimeUnrender _ bs =
        maybeToRight "Can't parse atom feed!" $ parseFeedSource bs

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight _ (Just a) = Right a
maybeToRight err Nothing = Left err

