{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DataModel.Acid where

import Data.Acid (makeAcidic)

import DataModel.Acid.Feed (addFeedIfUnique, listFeeds)
import DataModel.Type.DataModel (DataModel)

$(makeAcidic ''DataModel ['addFeedIfUnique, 'listFeeds])
