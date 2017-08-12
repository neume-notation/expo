-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.Types (
    GameData,
    Query(..),
    TalkSpec,
    TalkSequenceType
) where

import Data.Maybe                  (Maybe)

import Base.Types                  (Location, Language, TalkSequence)
import Base.Router                 (Routes, Extra)
import Base.ExpoData               (LocVideo, LocTalk, LocVidTime,
                                    LocUrl, LocHighlight, LocActive)
import Pages.Shared.TalkPage as Shared
import Pages.Shared.EndButtons as Nav
import Talk.Narrative as N

-- A one-off "find such-and-such" activity is defined by the active area
-- which is deemed correct, another area to make active which is deemed wrong
-- (the correct area overrides it) and a way of highlighting the correct
-- answer.  The "wrong" area is likely to be the whole visible part of the
-- image (the image may well have a blank part to help layout, which would
-- be omitted).  Note that an active area is built up from an array of
-- rectangles, so, in particular, the "right" answer may consist of several
-- distinct areas, any of which are taken as correct.
--
type GameData = { right  :: LocActive
                , wrong  :: LocActive
                , answer :: LocHighlight
                }

-- Query algebra type for Generic pages
--
-- CondAction is an unfortunate necessity for situations (eg Music) where the
-- videos mistakenly differ in a significant way between languages.  It allows
-- standard actions, particularly highlights, to depend on the language,
-- possibly only in the case of some locations.
--
data Query a =   Standard (Shared.StdAction Query) a
               | HandleMessage N.Msg a
               | Game GameData a
               | GameAnswered Boolean a
               | ShowAnswer a
               | ResetGame a
               | GoTo Nav.RouteSpec a
               | CondAction (Location -> Language -> Shared.StdAction Query) a

-- Types building in this components action type
--
type TalkSequenceType = TalkSequence Query

-- Specification of a page implemented by Generic.purs.  The data gives:
--
--     - The initial base image
--     - The talk itself, specified by:
--           video        :: LocVideo
--           transcript   :: LocTalk
--           timings      :: LocVidTime
--           sequence     :: Location -> TalkSequence
--     - An optional single-click, right or wrong game (for notation talks)
--       defined by the coordinates of the "right answer" active area, and
--       a highlight showing the correct answer (probably specified here as
--       the `a` value to lookup in the main set of highlights).
--     - The (statically known) Routes contructor for this page
--     - An optional continue page.
--
type TalkSpec = {
      base_image   :: LocUrl
    , video        :: LocVideo
    , transcript   :: LocTalk
    , timings      :: LocVidTime
    , sequence     :: Location -> TalkSequenceType
    , myself       :: Location -> Language -> Extra -> Routes
    , continue     :: Maybe (Location -> Language -> Extra -> Routes)
    }
