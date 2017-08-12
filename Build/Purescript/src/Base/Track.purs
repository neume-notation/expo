-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.Track (
    trackMe
) where

import Prelude

import Control.Monad.Aff           (forkAff)
import Data.Maybe                  (Maybe(..))
import Network.HTTP.Affjax         (get, Affjax)

import Base.Types                  (M)
import Base.Router                 (Routes, trackName)
import Base.ExpoData               (expoData)

trackMe :: forall eff. Maybe Routes -> Routes -> (M eff) Unit
trackMe prev route =
    -- The tracker URL already has a single-dot final part, because Yesod
    -- insists on a non-empty path piece: just append the identifier.
    -- We completely ignore the result, so use forkAff to avoid blocking.
    case trackName prev route of
      Just ident -> void $ forkAff $ pingServer ident
      Nothing    -> pure unit
  where
    pingServer :: forall e. String -> Affjax e Unit
    pingServer ident = get $ expoData.urls.tracker <> ident
