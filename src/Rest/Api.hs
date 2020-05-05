
module Rest.Api
    ()
  where

import Servant.API ((:>), Get)
import Servant.API.ContentTypes
    ( Accept(contentType)
    , MimeRender(mimeRender)
    , MimeUnrender(mimeUnrender)
    )
import Servant.Server (Handler, ServerT)
import Text.Atom.Feed
    ( Entry(entryLinks, entrySummary)
    , TextContent(TextString, HTMLString)
    , feedEntries
    , feedLinks
    , nullEntry
    , nullFeed
    , nullLink
    )

import Rest.AtomMime (AtomFeed)


type Api = "atom" :> Get '[AtomFeed] Feed
