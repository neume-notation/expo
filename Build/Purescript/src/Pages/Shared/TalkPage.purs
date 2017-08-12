-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Shared.TalkPage (
    StdAction(..),
    renderNarrative,
    showHighlight,
    renderHighlights,
    evalStandard,
    evalHidden,
    evalShowBackButton,
    evalHandleBackButton,
    videoList
) where

-- Parts of `render` and `eval` functions shared by all components which
-- show a talk and a video.
--
-- Note that the caller must have a state type consistent with BareTalkState
-- from Pages.Shared.Types or, for some functions, TalkState.

import Prelude

import Data.Array                  ((:))
import Data.Array as Arr
import Data.Map                    (insert, delete)
import Data.Maybe                  (Maybe(..), isNothing)
import Data.Nullable               (toMaybe)

import CSS.Geometry as Geometry
import CSS.Size as Size
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Talk.Narrative as N
import Talk.Dispatcher as D

import Base.Types                  (Location, Language(..), TalkSequence,
                                    Timing, M)
import Base.Router                 (Extra)
import Base.ExpoData               (expoData, pick, pickLoc, LocVideo,
                                    LocUrl, LocHighlight, Highlight(..))
import Base.CssClasses as Class
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Types as T


-- All talk pages include these standard actions as part of their query algebra
--
-- Initialize and Finalize set up and remove the dispatcher, and Initialize
-- also allows the initial base image to be set.  Reset sets an empty
-- set of highlights.  Actions are provided to change the following:
--
--     - The base image: Reset does not alter it, but a variant, ResetWithBase,
--       is provided to set the base image too for pages which make use of
--       multiple base images.
--     - Highlight images: these are all cleared on reset.
--
data StdAction q =   Initialize (Location -> TalkSequence q) (Maybe LocUrl)
                   | Finalize
                   | Reset
                   | SetBase LocUrl
                   | ResetWithBase LocUrl
                   | DoHighlight (Array LocHighlight)
                   | UnHighlight LocHighlight


-- The `videos`, `transcript`, and `timings` arguments are functions which
-- choose between languages; if the location needs to be selected too, that
-- should be done by the caller.  This is because language selection is done
-- for all talks, but location selection is only done for some.
--
renderNarrative ::
    forall eff q r s.
         s
      -> T.TalkState q r
      -> (Language -> N.VideoList)
      -> (Language -> String)
      -> (Language -> Timing)
      -> String
      -> (N.Msg -> H.Action q)
      -> Array (H.ParentHTML q N.Query s (M eff))
renderNarrative slot state videos transcript timings startAt handleMsg =
    if isNothing state.dispatcher then
        -- Do not attempt to render the Narrative component until the
        -- dispatcher has been initialized.  The first render does happen
        -- before the initializer is called (see the Halogen guide).
        -- TODO That was for Halogen 0.12 - is it still true?
        []
    else
        let init = { language:   state.language
                   , video:      videos state.language
                   , text:       transcript state.language
                   , timing:     timings state.language
                   , startAt:    startAt
                   , vidHidden:  state `T.hasOption` T.optHidden
                   , dispatcher: state.dispatcher
                   }
        in
        [ HH.slot slot N.ui init (HE.input handleMsg) ]

showHighlight :: forall p i. Location -> LocHighlight -> HH.HTML p i
showHighlight loc h =
    case toMaybe (pickLoc loc h) of
      Just (Highlight d) ->
        HH.div
          [ HP.class_ Class.highlight
          , CSS.style do Geometry.left (Size.px d.geom.x)
                         Geometry.top (Size.px d.geom.y)
          ]
          [ HH.img [ HP.src d.img ] ]
      Nothing ->
        HH.div_ []

renderHighlights :: forall q r p i. T.TalkState q r -> Array (HH.HTML p i)
renderHighlights state =
    map (showHighlight state.location) state.highlights

evalStandard ::
   forall eff n q r p.
        StdAction q
     -> n
     -> H.ParentDSL (T.TalkState q r) q N.Query p Void (M eff) n
evalStandard (Initialize talkSequence maybeBase) next = do
    -- Create the dispatcher
    loc <- H.gets _.location
    d <- H.liftEff $ D.newDispatcher (talkSequence loc)
    H.subscribe $ H.eventSource (D.onDispatch d) (\step -> do
        Just $ step.action H.Listening
    )
    case maybeBase of
      Just base -> H.modify (_ { base_image = base })
      Nothing -> pure unit
    -- NB Any `modify` can result in a re-render before the `eval` action is
    -- completed, so make sure the `dispatcher` field is not set until we are
    -- ready for `renderNarrative` to actually create a Narrative component,
    -- in particular having set up the eventsubscription.
    H.modify (_ { dispatcher = Just d })
    pure next

evalStandard Finalize next = do
    -- Make sure we are not keeping a reference to the dispatcher any longer
    H.modify (\s -> s { dispatcher = Nothing })
    pure next

evalStandard Reset next = do
    H.modify (_ { highlights = [] })
    pure next

evalStandard (SetBase base) next = do
    H.modify (_ { base_image = base })
    pure next

evalStandard (ResetWithBase base) next = do
    H.modify (_ { highlights = [], base_image = base })
    pure next

evalStandard (DoHighlight newOnes) next = do
    -- Add newOnes to highlights
    H.modify (\s -> s { highlights = s.highlights <> newOnes })
    pure next

evalStandard (UnHighlight toGo) next = do
    -- Remove a specific highlight
    H.modify (\s -> s { highlights = Arr.delete toGo s.highlights })
    pure next


-- Determine the current hidden-or-otherwise state of the video, adding or
-- removing the "hidden" key from the given Extra value.
-- Note that the slot type for the Narrative is forced to be Unit here - as
-- it is in the talk pages, in fact.
--
evalHidden :: forall eff q r.
    Extra
    -> H.ParentDSL (T.TalkState q r) q N.Query Unit Void (M eff) Extra
evalHidden e = do
     hide <- H.query unit $ H.request N.IsHidden
     let extra = if hide == Just true
                   then insert T.optHidden "y" e
                   else delete T.optHidden e
     pure extra

-- Show the back button if we have come from the Intro, ie if we have a
-- "pos" in the options, but not otherwise
--
evalShowBackButton :: forall eff q r.
    H.ParentDSL (T.TalkState q r) q N.Query Unit Void (M eff) Unit
evalShowBackButton = do
    s <- H.get
    if s `T.hasOption` T.optIntroPos
      then do
          let classes = case s.language of
                          En -> [ Class.dark_green_button ]
                          _  -> [ Class.dark_green_button, Class.small_text ]
          _ <- H.query unit
            $ H.action (N.DetailButtons classes
                          (pick s.language expoData.messages.back_button)
                          "back"
                       )
          pure unit
      else pure unit

-- Talk-page action to handle a narrative message which is known to be a "back"
-- button click.  We must update the 'hidden' state, and resume the intro.
--
evalHandleBackButton :: forall eff a q r.
    a -> H.ParentDSL (T.TalkState q r) q N.Query Unit Void (M eff) a
evalHandleBackButton next = do
    options <- H.gets _.options
    updated <- evalHidden options
    H.modify ( _{options = updated} )
    Nav.evalResume next

-- Utility to convert a video specifier (of type LocVideo) to N.VideoList,
-- given the location and language required.
--
videoList :: LocVideo -> Location -> Language -> N.VideoList
videoList video loc lang =
    let mp4part = case toMaybe (video.mp4) of
                    Nothing  -> []
                    Just vid -> [ { url: pick lang $ pickLoc loc vid
                                  , mimeType: "video/mp4"
                                  }
                                ]
    in  { url: pick lang $ pickLoc loc video.webm
        , mimeType: "video/webm"
        }
        : mp4part
