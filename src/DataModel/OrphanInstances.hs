{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module DataModel.OrphanInstances
    ()
  where

import Data.SafeCopy (base, deriveSafeCopy)
import Network.URI (URI, URIAuth)


$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
