-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Shared.Types (
    PageState,
    TalkState,
    hasOption,
    option,
    optIntroPos,
    optHidden,
    optCredits
) where

import Data.Map                    (member, lookup)
import Data.Maybe                  (Maybe)
import Talk.Dispatcher as D
import Base.Types                  (Location, Language)
import Base.ExpoData               (LocUrl, LocHighlight)
import Base.Router                 (Extra)

-- All pages must have a state at least as general as this.  The `options`
-- are URL query parameters, and for all content pages may include these keys:
--
--     "pos"      The position within the Intro, used when returning to the
--                intro from sub-pages using a "back" button.  This is only
--                ever set when Intro component changes the URL to another
--                page, and most other pages simply propagate it unchanged.
--                However, the Start and Explore pages do not propagate it,
--                so the intro starts from the beginning, and, in fact, cannot
--                be reached from Exlore without going through the start page.
--
--     "hidden"   Present if the video is hidden.  All talk pages can set this
--                or remove it, but in the absence of explicit action, all
--                pages other than Start and Explore propagate it.
--
-- Some forms of Start page URL can result in "choose" being added to Extra,
-- but it is never added as a query parameter and so will never be propagated
-- even if `options` are simply passed on in a new URL.
--
-- The Explore page can made to show different content by setting the key
-- "credits" with any value.
--
type PageState r = { location   :: Location
                   , language   :: Language
                   , options    :: Extra
                   | r
                   }

-- All talk-and-video pages have additional fields for the Dispatcher,
-- base image (on the left hand side) and highlights.
--
type TalkState q r = PageState (
                           dispatcher :: Maybe (D.Dispatcher q)
                         , base_image :: LocUrl
                         , highlights :: Array LocHighlight
                         | r
                     )

hasOption :: forall r. PageState r -> String -> Boolean
hasOption state opt = member opt state.options

option :: forall r. PageState r -> String -> Maybe String
option state opt = lookup opt state.options

optIntroPos :: String
optIntroPos = "pos"

optHidden :: String
optHidden = "hidden"

optCredits :: String
optCredits = "credits"
