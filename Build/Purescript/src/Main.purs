-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Main where

-- This module serves as the router of our single page application.  It is
-- a Halogen component, and it renders a little boilerplate plus the slots
-- for the components representing the alternative views: it is merely the
-- parent of the various pages which actually display.

import Prelude

import Control.Monad.Aff           (forkAff)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Class     (liftEff)
import Data.Either                 (Either)
import Data.Map                    (empty, singleton)
import Data.Functor.Coproduct      (Coproduct)
import Data.Maybe                  (Maybe(..))
import Data.Tuple                  (Tuple(Tuple))
import Data.Traversable            (traverse)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB
import Halogen.Aff                 (HalogenIO, awaitBody, runHalogenAff)
import Halogen.VDom.Driver         (runUI)

import Base.Types                  (Language, ExpoEffects, M)
import Base.Router                 (Routes(..), dfltRoute,
                                    setRoute, getRoute, watchRoutes)
import Base.ExpoData               (expoData, pick)
import Base.Track                  (trackMe)
import Pages.Shared.Types          (optCredits)

import Pages.Start    as Start
import Pages.Explore  as Explore
import Pages.Intro    as Intro
import Pages.Generic  as Generic
import Pages.Survival as Survival
import Pages.Phrases  as Phrases
import Pages.TryCEAP  as TryCEAP
import Pages.ReadText as ReadText

import Pages.Generic.Codex    as Codex
import Pages.Generic.UseOfMS  as UseOfMS
import Pages.Generic.Music    as Music
import Pages.Generic.Cadence  as Cadence
import Pages.Generic.TextBits as TextBits
import Pages.Generic.TextMore as TextMore


-- Top-level parent component controls children according to route
-- ---------------------------------------------------------------

-- The child components implement the actual pages, and there is only one
-- of them in the DOM at any time.  This policy is in keeping with the
-- normal Halogen / VDom approach of rendering everything according
-- to the current state, and optimising it behind the scenes.  It makes it
-- clear that none of the state is hidden in the DOM itself.  Performance
-- is very good, so there is no reason to do anything different.

newtype StartSlot = StartSlot Int
derive newtype instance eqStartSlot :: Eq StartSlot
derive newtype instance ordStartSlot :: Ord StartSlot

newtype ExploreSlot = ExploreSlot Int
derive newtype instance eqExploreSlot :: Eq ExploreSlot
derive newtype instance ordExploreSlot :: Ord ExploreSlot

newtype IntroSlot = IntroSlot Int
derive newtype instance eqIntroSlot :: Eq IntroSlot
derive newtype instance ordIntroSlot :: Ord IntroSlot

newtype GenericSlot = GenericSlot Int
derive newtype instance eqGenericSlot :: Eq GenericSlot
derive newtype instance ordGenericSlot :: Ord GenericSlot

newtype SurvivalSlot = SurvivalSlot Int
derive newtype instance eqSurvivalSlot :: Eq SurvivalSlot
derive newtype instance ordSurvivalSlot :: Ord SurvivalSlot

newtype PhrasesSlot = PhrasesSlot Int
derive newtype instance eqPhrasesSlot :: Eq PhrasesSlot
derive newtype instance ordPhrasesSlot :: Ord PhrasesSlot

newtype TryCEAPSlot = TryCEAPSlot Int
derive newtype instance eqTryCEAPSlot :: Eq TryCEAPSlot
derive newtype instance ordTryCEAPSlot :: Ord TryCEAPSlot

newtype ReadTextSlot = ReadTextSlot Int
derive newtype instance eqReadTextSlot :: Eq ReadTextSlot
derive newtype instance ordReadTextSlot :: Ord ReadTextSlot

type ChildQuery = Coproduct
                        ( Coproduct (Coproduct Start.Query Explore.Query)
                                    (Coproduct Intro.Query Generic.Query)
                        )
                        ( Coproduct (Coproduct Survival.Query Phrases.Query)
                                    (Coproduct TryCEAP.Query ReadText.Query)
                        )
type ChildSlot  =    Either
                        ( Either (Either StartSlot ExploreSlot)
                                 (Either IntroSlot GenericSlot)
                        )
                        ( Either (Either SurvivalSlot PhrasesSlot) 
                                 (Either TryCEAPSlot ReadTextSlot)
                        )

childStart :: ChildPath Start.Query ChildQuery StartSlot ChildSlot
childStart = cpL :> cpL :> cpL

childExplore :: ChildPath Explore.Query ChildQuery ExploreSlot ChildSlot
childExplore = cpL :> cpL :> cpR

childIntro :: ChildPath Intro.Query ChildQuery IntroSlot ChildSlot
childIntro = cpL :> cpR :> cpL

childGeneric :: ChildPath Generic.Query ChildQuery GenericSlot ChildSlot
childGeneric = cpL :> cpR :> cpR

childSurvival :: ChildPath Survival.Query ChildQuery SurvivalSlot ChildSlot
childSurvival = cpR :> cpL :> cpL

childPhrases :: ChildPath Phrases.Query ChildQuery PhrasesSlot ChildSlot
childPhrases = cpR :> cpL :> cpR

childTryCEAP :: ChildPath TryCEAP.Query ChildQuery TryCEAPSlot ChildSlot
childTryCEAP = cpR :> cpR :> cpL

childReadText :: ChildPath ReadText.Query ChildQuery ReadTextSlot ChildSlot
childReadText = cpR :> cpR :> cpR


-- `serial` is incremented on any change of route, other than query params,
-- and is used to construct unique slot values.
type State = { page     :: Routes
             , serial   :: Int
             }

initialState :: State
initialState = { page:     dfltRoute
               , serial:   0
               }

data Query a =   GoTo Routes Boolean a
               | HandleStart Start.Msg a


-- Pass the given content through, adding a message instructing the user
-- on how to adjust their browser for viewing this exhibition on a small
-- screen.  This organisation allows us to get the language from the
-- decoded Routes in setPage.  The message is a full-window overlay,
-- forced to the top with a z-index, so it prevents any other action until
-- the user has resized and/or zoomed sufficiently to satisfy the size
-- constraints.  It is always generated, but the class ".screentoosmall"
-- causes it *not* to be displayed for windows which are large enough (as
-- defined in expo.css).  Note this is done on all pages in case the user
-- resizes their window or follows a deep link into this site.
--
smallSizeMsg :: forall p i. Language -> HH.HTML p i -> Array (HH.HTML p i)
smallSizeMsg lang content =
    [ HH.div
      [ HP.class_ $  HH.ClassName "screentoosmall" ]
      [ HH.div_
        [ HH.p_
          [ HH.text $ pick lang expoData.messages.small_screen ]
        , HH.ol_
          [ HH.li_
            [ HH.text $ pick lang expoData.messages.small_full ]
          , HH.li_
            [ HH.text $ pick lang expoData.messages.small_zoom ]
          ]
        ]
      ]
    , content
    ]


ui :: forall eff. H.Component HH.HTML Query Unit Void (M eff)
ui = H.parentComponent {
       initialState: const initialState
     , render
     , eval
     , receiver: const Nothing
     }
  where
    -- Top-level pages must have class "expo-page" and be Bootstrap containers
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (M eff)
    render state =
      HH.div
        [ HP.classes [HB.containerFluid, HH.ClassName "expo-page"] ]
        $ setPage state.page state

    -- If the location and/or language is changed by an explicit URL change,
    -- while staying on the same page, the slot is changed (see `modifyState`
    -- below) and so the child component is replaced by a new copy with the
    -- new location and language.
    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (M eff)
    eval (GoTo route changed next) = do
      H.modify (modifyState route changed)
      pure next
    eval (HandleStart (Start.SetRoute route) next) = do
      -- The route needs to be changed to point to one passed in the SetRoute
      -- message from the start page.  The route is set directly, and the
      -- resulting hashchanged event will be fed back into Halogen, which
      -- will change the displayed page.
      H.liftEff $ setRoute route
      pure next

    -- Child components are initialised with the correct location and language,
    -- and are removed and replaced (by changing the slot value) if those
    -- parameters are altered by an explicit URL change.  This avoids any
    -- potential issues over resetting the "third party" components, since
    -- they are re-created on any significant change.  However, it would be
    -- possible to handle all of this through the input/receiver mechanism.
    --
    setPage (Start loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childStart
                   (StartSlot state.serial)
                   Start.ui
                   { location: loc, language: lang, options: extra }
                   (HE.input HandleStart)

    setPage (Explore loc lang _) state =
      smallSizeMsg lang
        $ HH.slot' childExplore
                   (ExploreSlot state.serial)
                   Explore.ui
                   { location: loc, language: lang, options: empty }
                   absurd

    setPage (Credits loc lang _) state =
      -- The credits page is implemented by the Explore component.  The serial
      -- number changes, changing the slot, so a new component is created.
      let doCredits = singleton optCredits ""
      in
      smallSizeMsg lang
        $ HH.slot' childExplore
                   (ExploreSlot state.serial)
                   Explore.ui
                   { location: loc, language: lang, options: doCredits }
                   absurd

    setPage (Intro loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childIntro
                   (IntroSlot state.serial)
                   Intro.ui
                   { location: loc, language: lang, options: extra }
                   absurd

    setPage (Survival loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childSurvival
                   (SurvivalSlot state.serial)
                   Survival.ui
                   { location: loc, language: lang, options: extra }
                   absurd

    setPage (Phrases loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childPhrases
                   (PhrasesSlot state.serial)
                   Phrases.ui
                   { location: loc, language: lang, options: extra }
                   absurd

    setPage (TryCEAP loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childTryCEAP
                   (TryCEAPSlot state.serial)
                   TryCEAP.ui
                   { location: loc, language: lang, options: extra }
                   absurd

    setPage (ReadText loc lang extra) state =
      smallSizeMsg lang
        $ HH.slot' childReadText
                   (ReadTextSlot state.serial)
                   ReadText.ui
                   { location: loc, language: lang, options: extra }
                   absurd

    setPage (Codex loc lang extra) state =
        setGeneric Codex.spec loc lang extra state
    setPage (UseOfMS loc lang extra) state =
        setGeneric UseOfMS.spec loc lang extra state
    setPage (Music loc lang extra) state =
        setGeneric Music.spec loc lang extra state
    setPage (Cadence loc lang extra) state =
        setGeneric Cadence.spec loc lang extra state
    setPage (TextBits loc lang extra) state =
        setGeneric TextBits.spec loc lang extra state
    setPage (TextMore loc lang extra) state =
        setGeneric TextMore.spec loc lang extra state

    setGeneric spec loc lang extra state =
      smallSizeMsg lang
        $ HH.slot' childGeneric
                   (GenericSlot state.serial)
                   (Generic.makeUI spec)
                   { location: loc, language: lang, options: extra }
                   absurd

-- Helper for `eval` function of ui
modifyState :: Routes -> Boolean -> State -> State
modifyState page changed oldstate =
    let newserial = if changed then oldstate.serial + 1 else oldstate.serial
    in
    oldstate { page   = page
             , serial = newserial
             }


-- Tell the Halogen UI about route changes
-- ---------------------------------------

-- We use the "IO" for the top-level UI component to tell the UI what
-- state to go into for the new route.  `routeChanges` simply calls
-- `watchRoutes` with a continuation which passes the changes into the ui.
-- Since `watchRoutes` is just a thin wrapper around `matchesAff` from
-- purescript-routing, it invokes the continuation every time the route
-- changes.
routeChanges :: forall eff. HalogenIO Query Void (M eff) -> M eff Unit
routeChanges expoIO = do
    Tuple old new <- watchRoutes
    newUIState expoIO old new

-- `newUIState` is the actual action to perform on each route change: it
-- calls the `query` part of the IO with the correct action, translated
-- from the route information - the "translation" is rather trivial here,
-- but it does determine whether the route has changed in a significant way.
-- Note that equality of Routes ignores the "Extra", so the route has
-- "changed" only if the constructor, location or language has changed.
newUIState :: forall eff. HalogenIO Query Void (M eff)
                          -> Maybe Routes
                          -> Routes
                          -> M eff Unit
newUIState expoIO prev route = do
    trackMe prev route
    let changed = case prev of
                    Just p  -> route /= p
                    Nothing -> true
    expoIO.query (H.action (GoTo route changed))


main :: Eff (ExpoEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody

  expoIO <- runUI ui unit body

  -- Get the initial Route, if possible, and initialise the UI with it.
  -- This does seem to be necessary to avoid the default state being used
  -- at the start regardless of the URL fragment given.
  -- Note that a web user might start with any valid URL fragment and query
  -- string, either by following a link or by forcing a reload.
  route <- liftEff getRoute
  _ <- traverse (newUIState expoIO Nothing) route   -- ie traverse over Maybe

  forkAff $ routeChanges expoIO
