-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.UserPref (
    language,
    locationString,
    location,
    timeout
) where

import Prelude

import Data.Maybe                  (Maybe(..))
import Base.Types                  (Location(..))

-- Best guess at the user's preferred language.  The exact meaning depends
-- on the browser.  The value is a lower-case, two-letter code, such as
-- "en", "es", "pt" etc

foreign import language :: String

-- Static, install-time location (defaults to "", which is taken as unset)
foreign import locationString :: String

-- Version of location translated to Location type - note this can only
-- give a Just result for those locations which have manuscripts, so
-- in some situations locationString may need to be checked as well.
location :: Maybe Location
location =
    if      locationString == "Coimbra"  then Just Coimbra
    else if locationString == "Lamego"   then Just Lamego
    else if locationString == "Leon"     then Just Leon
    else if locationString == "Madrid"   then Just Madrid
    else Nothing

-- The fast, special-case timeout for the Explore and Credits pages, in
-- seconds (defaults to 30)
foreign import timeout :: Int
