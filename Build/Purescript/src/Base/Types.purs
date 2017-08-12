-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.Types (
    Location(..),
    Language(..),
    dfltLang,
    Timing(..),
    DispatchAction,
    TalkElement,
    TalkSequence(..),
    ExpoEffects,
    M
) where

import Prelude

import Control.Monad.Aff           (Aff)
import DOM.HTML.Types              (WINDOW)
import Halogen                     (SubscribeStatus)
import Halogen.Aff                 (HalogenEffects)
import Network.HTTP.Affjax         (AJAX)

-- Location really defines which manuscript is being used in those parts of
-- the presentation in which the locally-displayed manuscript is used for
-- highlighting examples.
--
data Location = Coimbra | Lamego | Leon | Madrid

-- Use lower case for compatibility with routing URLs
instance showLocation :: Show Location where
    show Coimbra = "coimbra"
    show Lamego  = "lamego"
    show Leon    = "leon"
    show Madrid  = "madrid"

derive instance eqLocation :: Eq Location

data Language = En | Es | Pt

instance showLanguage :: Show Language where
    show En = "en"
    show Es = "es"
    show Pt = "pt"

derive instance eqLanguage :: Eq Language

-- Default language for each location
dfltLang :: Location -> Language
dfltLang Coimbra = Pt
dfltLang Lamego  = Pt
dfltLang Leon    = Es
dfltLang Madrid  = Es

-- Timings used by the Narrative widget.  Defined here since they come from
-- the server and are declared in ExpoData.
--
newtype Timing = Timing (Array { name :: String, time :: Int })

-- When used via the dispatcher, the actions of the query algebra need to
-- hold a value of type SubscribeStatus instead of Unit (which is what
-- the type H.Action would build in).  See `evalStandard (Initialize...`
-- in TalkPage.purs.
--
type DispatchAction q = SubscribeStatus -> q SubscribeStatus

type TalkElement q = { name  :: String , reset :: Boolean, action :: DispatchAction q }

newtype TalkSequence q = TalkSequence (Array (TalkElement q))

-- Some (most) of our components need effects beyond transforming internal
-- state, so we work in the effect monad defined here - this is used for the
-- type parameter the documentation calls `m` in the general case.  See
-- https://github.com/slamdata/purescript-halogen/blob/master/docs/3%20-%20Handling%20effects.md
--
type ExpoEffects eff = (ajax :: AJAX, window :: WINDOW | HalogenEffects eff)
type M eff = Aff (ExpoEffects eff)
