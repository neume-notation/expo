-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.Router (
    Routes(..),
    Extra,
    trackName,
    dfltRoute,
    setRoute,
    parseRoute,
    getRoute,
    watchRoutes,
    homeRoute,
    restartRoute,
    introRoute
) where

import Prelude

import Control.Alt                 ((<|>))
import Control.Monad.Aff           (Aff)
import Control.Monad.Eff           (Eff)
import Data.Either                 (Either(..))
import Data.Map                    (Map, empty, singleton, member, delete,
                                    isEmpty, toUnfoldable)
import Data.Maybe                  (Maybe(..))
import Data.String                 (joinWith)
import Data.Tuple                  (Tuple(..))
import DOM                         (DOM)

import Routing                     (match, matchesAff)
import Routing.Hash                (getHash, setHash)
import Routing.Match               (Match)
import Routing.Match.Class         (lit, params)

import Base.Types                  (Location(..), Language(..), dfltLang)
import Base.UserPref as U


-- Routes for this application
-- ---------------------------

-- Note that Routing splits on slashes (as I understand it), so we are
-- matching against a list of path pieces.  The first piece is empty
-- if the URL paths look like /server/path#/client/path - obviously the
-- slash at the start of /client/path is something we could decide to
-- omit.
--
-- The guide to purescript-routing is at
--     https://github.com/slamdata/purescript-routing/blob/master/GUIDE.md
-- and a useful tutorial on using it with Halogen can be found at
--     http://www.parsonsmatt.org/2015/10/22/purescript_router.html
-- (but be careful of details which are a little out of date).


-- We'll call this type "Routes", but beware of the singular "Route" exposed
-- by purescript-routing.  The Extra field is a `Map String String` which
-- contains query parameters or other decoded url information. In the case
-- of the Start route, it contains the key "choose" iff the location chooser
-- should be displayed on the start page.  In the case of Intro, the key "pos"
-- can give the timing step at which to start, and is used when resuming the
-- introduction after going to a subsidiary page.  For all talk pages, the
-- presence of the key "hidden" indicates that the video should be hidden
-- initially.  See also the comment in Pages/Shared/Types.purs
type Extra = Map String String
data Routes =
      Start     Location Language Extra
    | Explore   Location Language Extra
    | Credits   Location Language Extra
    | Intro     Location Language Extra
    | Codex     Location Language Extra
    | UseOfMS   Location Language Extra
    | Survival  Location Language Extra
    | Music     Location Language Extra
    | Cadence   Location Language Extra
    | Phrases   Location Language Extra
    | TryCEAP   Location Language Extra
    | TextBits  Location Language Extra
    | TextMore  Location Language Extra
    | ReadText  Location Language Extra

-- Equality does *not* depend on the query parameters (ie the Extra)
instance eqRoutes :: Eq Routes where
    eq (Start     x y _) (Start     u v _) = x == u && y == v
    eq (Explore   x y _) (Explore   u v _) = x == u && y == v
    eq (Credits   x y _) (Credits   u v _) = x == u && y == v
    eq (Intro     x y _) (Intro     u v _) = x == u && y == v
    eq (Codex     x y _) (Codex     u v _) = x == u && y == v
    eq (UseOfMS   x y _) (UseOfMS   u v _) = x == u && y == v
    eq (Survival  x y _) (Survival  u v _) = x == u && y == v
    eq (Music     x y _) (Music     u v _) = x == u && y == v
    eq (Cadence   x y _) (Cadence   u v _) = x == u && y == v
    eq (Phrases   x y _) (Phrases   u v _) = x == u && y == v
    eq (TryCEAP   x y _) (TryCEAP   u v _) = x == u && y == v
    eq (TextBits  x y _) (TextBits  u v _) = x == u && y == v
    eq (TextMore  x y _) (TextMore  u v _) = x == u && y == v
    eq (ReadText  x y _) (ReadText  u v _) = x == u && y == v
    eq _ _ = false

extraChoose :: Extra
extraChoose = singleton "choose" ""

dfltLoc :: Location
dfltLoc = Madrid

dfltRoute :: Routes
dfltRoute = Start dfltLoc lang extraChoose
  where
    lang = if U.language == "es" then Es
           else if U.language == "pt" then Pt
           else En

-- Note that the key "choose" in Extra is treated specially in showRoute, and
-- is never included in a query string.
showExtra :: Extra -> String
showExtra e =
    if isEmpty reduced
      then ""
      else "/?" <> (joinWith "&" $ map oneParam $ toUnfoldable reduced)
  where
    reduced = delete "choose" e
    oneParam (Tuple k v) = k <> "=" <> v

-- Show gives the correct form for the URL fragment
-- The `pos` parameter to Intro is not checked for URL special characters,
-- but it is only ever generated internally, so can be assumed safe.
instance showRoute :: Show Routes where
    show (Start loc lang e) =
        if member "choose" e then "/choice/" <> show lang
        else "/start/" <> show loc <> "/" <> show lang
    show (Explore loc lang e) =
        "/explore/" <> show loc <> "/" <> show lang <> showExtra e
    show (Credits loc lang e) =
        "/credits/" <> show loc <> "/" <> show lang <> showExtra e
    show (Intro loc lang e) =
        "/intro/" <> show loc <> "/" <> show lang <> showExtra e
    show (Codex loc lang e) =
        "/codicology/" <> show loc <> "/" <> show lang <> showExtra e
    show (UseOfMS loc lang e) =
        "/useofms/" <> show loc <> "/" <> show lang <> showExtra e
    show (Survival loc lang e) =
        "/survival/" <> show loc <> "/" <> show lang <> showExtra e
    show (Music loc lang e) =
        "/music/" <> show loc <> "/" <> show lang <> showExtra e
    show (Cadence loc lang e) =
        "/cadence/" <> show loc <> "/" <> show lang <> showExtra e
    show (Phrases loc lang e) =
        "/phrases/" <> show loc <> "/" <> show lang <> showExtra e
    show (TryCEAP loc lang e) =
        "/tryceap/" <> show loc <> "/" <> show lang <> showExtra e
    show (TextBits loc lang e) =
        "/textbits/" <> show loc <> "/" <> show lang <> showExtra e
    show (TextMore loc lang e) =
        "/textmore/" <> show loc <> "/" <> show lang <> showExtra e
    show (ReadText loc lang e) =
        "/readtext/" <> show loc <> "/" <> show lang <> showExtra e

-- Should the given URL transition be recorded (via logging on a server)
-- for statistics gathering, and if so, with what (single-character) name?
-- The old route is always available in the cases which matter, and is
-- provided as the first parameter, and the target route is the second
-- parameter.  The result is Nothing when no tracking is needed.
--
-- In the case of the Intro, it is important to only record the entries
-- from the Start page, since we do not want to record people using a
-- "back" button to return to it.  Entering any other page is recorded
-- whatever the previous route, since this captures coming in via
-- the Explore menu, Continue where provided, and Intro-page "more detail"
-- buttons.
--
-- Avoid names consisting of any of the letters Q,S,T,U, or any digit, since
-- the stand-alone software puts these into the log for its own purposes.
--
trackName :: Maybe Routes -> Routes -> Maybe String
trackName (Just (Start _ _ _))   (Intro _ _ _)      = Just "I"
trackName (Just _)               (Explore _ _ _)    = Just "E"
trackName (Just _)               (Credits _ _ _)    = Just "x"
trackName (Just _)               (Codex _ _ _)      = Just "C"
trackName (Just _)               (UseOfMS _ _ _)    = Just "u"
trackName (Just _)               (Survival _ _ _)   = Just "s"
trackName (Just _)               (Music _ _ _)      = Just "M"
trackName (Just _)               (Cadence _ _ _)    = Just "c"
trackName (Just _)               (Phrases _ _ _)    = Just "p"
trackName (Just _)               (TryCEAP _ _ _)    = Just "t"
trackName (Just _)               (TextBits _ _ _)   = Just "W"
trackName (Just _)               (TextMore _ _ _)   = Just "l"
trackName (Just _)               (ReadText _ _ _)   = Just "r"
trackName _                      _                  = Nothing

-- `params` seems to require at least one parameter, ie "/?" is not sufficient;
-- anyway it's cleaner to avoid the trailing "/?" completely.
extra :: Match Extra
extra =
        params
    <|> pure empty

language :: Match Language
language =
        En <$ lit "en"
    <|> Es <$ lit "es"
    <|> Pt <$ lit "pt"

location :: Match Location
location =
        Coimbra <$ lit "coimbra"
    <|> Lamego <$ lit "lamego"
    <|> Leon <$ lit "leon"
    <|> Madrid <$ lit "madrid"

-- Start page with defaults for both location and language
home :: Match Routes
home = dfltRoute <$ lit ""

-- Start page for given location, using default language for that location
startDfltLang :: Match Routes
startDfltLang = startPage <$> (lit "" *> lit "start" *> location)
  where
    startPage loc = Start loc (dfltLang loc) empty

-- Start page explcitly requesting location choice page
startChoice :: Match Routes
startChoice = Start dfltLoc <$> (lit "" *> lit "choice" *> language) <*> pure extraChoose

-- Start page for given location and language
start :: Match Routes
start = Start <$> (lit "" *> lit "start" *> location) <*> language <*> pure empty

-- Explore-further navigation page for given location, language
explore :: Match Routes
explore = Explore <$> (lit "" *> lit "explore" *> location)
                  <*> language <*> pure empty

-- Credits page for given location, language
credits :: Match Routes
credits = Credits <$> (lit "" *> lit "credits" *> location)
                  <*> language <*> pure empty

-- Main page for given location, language and step at which to start
intro :: Match Routes
intro = Intro <$> (lit "" *> lit "intro" *> location) <*> language <*> extra

-- Codicology - what is the MS like
codex :: Match Routes
codex = Codex <$> (lit "" *> lit "codicology" *> location) <*> language <*> extra

-- Codicology - use of the manuscript
useofms :: Match Routes
useofms = UseOfMS <$> (lit "" *> lit "useofms" *> location) <*> language <*> extra

-- Codicology - guess where manuscripts have been found
survival :: Match Routes
survival = Survival <$> (lit "" *> lit "survival" *> location) <*> language <*> extra

-- Notation - introductory
music :: Match Routes
music = Music <$> (lit "" *> lit "music" *> location) <*> language <*> extra

-- Notation - cadences
cadence :: Match Routes
cadence = Cadence <$> (lit "" *> lit "cadence" *> location) <*> language <*> extra

-- Notation - find places in the text where pauses are likely to occur
phrases :: Match Routes
phrases = Phrases <$> (lit "" *> lit "phrases" *> location) <*> language <*> extra

-- Notation - try transcribing in a very cut down CEAP-ike interface
tryceap :: Match Routes
tryceap = TryCEAP <$> (lit "" *> lit "tryceap" *> location) <*> language <*> extra

-- Text - elements
textbits :: Match Routes
textbits = TextBits <$> (lit "" *> lit "textbits" *> location) <*> language <*> extra

-- Text - more detailed look
textmore :: Match Routes
textmore = TextMore <$> (lit "" *> lit "textmore" *> location) <*> language <*> extra

-- Text - help / try reading it (interactive)
readtext :: Match Routes
readtext = ReadText <$> (lit "" *> lit "readtext" *> location) <*> language <*> extra

-- Note that the variants of "start" are resolved by the ordering
routing :: Match Routes
routing =
        explore
    <|> credits
    <|> intro
    <|> codex
    <|> useofms
    <|> survival
    <|> music
    <|> cadence
    <|> tryceap
    <|> phrases
    <|> textbits
    <|> textmore
    <|> readtext
    <|> start
    <|> startChoice
    <|> startDfltLang
    <|> home

-- Utility functions
-- -----------------

setRoute :: forall eff. Routes -> Eff (dom :: DOM | eff) Unit
setRoute r = setHash $ show r

parseRoute :: String -> Maybe Routes
parseRoute hash =
    case match routing hash of
      Right route -> Just route
      _           -> Nothing

getRoute :: forall e. Eff (dom :: DOM | e) (Maybe Routes)
getRoute = do
    hash <- getHash
    pure $ parseRoute hash

watchRoutes :: forall e. Aff e (Tuple (Maybe Routes) Routes)
watchRoutes = matchesAff routing

-- The hash part of the URL for the default start page for this installation.
-- This route is used only for timeouts, and agrees with the global timer in
-- the html head - in other cases use either `introRoute` or `restartRoute`.
-- The value is embedded in the html head of the html, and is configured by
-- the small file standalone_params.js at exhibition sites.
--
foreign import homeHash :: String

homeRoute :: Routes
homeRoute = case match routing homeHash of
                Right route -> route
                _           -> dfltRoute

-- restartRoute determines the appropriate route to use for the start page,
-- while preserving the language.  At sites where the location is defined
-- statically, that location is used, otherwise the start page with a choice
-- of location is used.
restartRoute :: Language -> Routes
restartRoute lang =
    case U.location of
        Just fixed -> Start fixed lang empty
        Nothing    -> Start dfltLoc lang extraChoose

-- introRoute determines the appropriate route to use for the introduction.
-- At sites where the location is defined statically, that definition
-- overrides the dynamic location.
introRoute :: Location -> Language -> Extra -> Routes
introRoute loc lang e =
    case U.location of
        Just fixed -> Intro fixed lang e
        Nothing    -> Intro loc lang e
