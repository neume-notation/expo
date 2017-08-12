-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic (
    module Pages.Generic.Types,    -- To get Query exported
    makeUI
) where

-- This component is a generic talk page, which is sufficiently general for
-- all of the video talks other than the introduction.
--
-- The `makeUI` function takes a TalkSpec parameter, as defined in
-- Generic/Types.purs

import Prelude

import Data.Array                  ((:))
import Data.Maybe                  (Maybe(..), isJust)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Base.Types                  (Location, Language, Timing, M)
import Base.ExpoData               (expoData, pick, pickLoc, Active(..))
import Base.CssClasses as Class
import Pages.Generic.Types         (Query(..), TalkSpec, GameData)
import Pages.Shared.TalkPage       (StdAction(..))
import Pages.Shared.TalkPage as Shared
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T
import Pages.Shared.Activities as A
import Talk.Narrative as N


-- The state of the game - if one is present at all - is determined by the
-- combination of `answered` and `showAnswer`:
--
--     showAnswer == true   The answer highlight should be shown, and the game
--                          is finished (there are no active areas or buttons).
--                          If `answered` is true, the user guessed correctly,
--                          otherwise they pressed the "I give up" button.
--
--     showAnswer == false  The game is still active, so the answer highlight
--                          is not shown.  If `answered` is true, the user
--                          has attempted an answer which was incorrect,
--                          otherwise the game is completely fresh: this
--                          state is cleared (ie `answered` is set false)
--                          after a timeout (implemented by the transitionend
--                          event on the display of feedback to the user).
--
type State = T.TalkState Query (
               game       :: Maybe GameData
             , answered   :: Boolean
             , showAnswer :: Boolean
             )

initialState :: T.PageState () -> State
initialState init = {
                 location:   init.location
               , language:   init.language
               , options :   init.options
               , base_image: expoData.locUrls.intro_ms -- Dummy, but valid image
               , highlights: []
               , game:       Nothing
               , answered:   false
               , showAnswer: false
               , dispatcher: Nothing
               }

-- There's only one child (necessarily of a single type (!)), uniquely tied
-- to this component, so the slot address needs only one value.
type NarrativeSlot = Unit

-- Set the active areas, or the answer, for the game, if there is a game.
--
-- The two possibilities are mutially exclusive: if the answer is being
-- shown, then no area is active, and vice versa.
--
gameArea :: forall eff. State -> Array (H.ParentHTML Query N.Query NarrativeSlot (M eff))
gameArea state =
    case state.game of
      Just g  -> if not state.showAnswer
                   then
                     activeArea g.right true
                     <> activeArea g.wrong false
                   else [ Shared.showHighlight state.location g.answer ]
      Nothing -> []
  where
    activeArea area isCorrect =
        case pickLoc state.location area of
          Active active -> A.activeRectangle isCorrect GameAnswered <$> active


gameControls :: forall eff. State -> Array (H.ParentHTML Query N.Query NarrativeSlot (M eff))
gameControls state =
    if isJust state.game then
        if state.showAnswer then
            if state.answered then
                A.showEmoji A.Happy Nothing
            else
                A.showEmoji A.Blank Nothing
        else
            if state.answered then
                A.showEmoji A.Sad (Just ResetGame)
                <> A.giveUpButton state ShowAnswer
            else
                A.showEmoji A.SadFade Nothing
                <> A.giveUpButton state ShowAnswer
    else []


makeUI :: forall eff.
            TalkSpec -> H.Component HH.HTML Query (T.PageState ()) Void (M eff)
makeUI talkSpec =
    H.lifecycleParentComponent {
      initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action (
                              Standard $ Initialize talkSpec.sequence
                                                  $ Just talkSpec.base_image
                          )
    , finalizer: Just $ H.action (Standard Finalize)
    }
  where
    render :: State -> H.ParentHTML Query N.Query NarrativeSlot (M eff)
    render state =
      let loc = state.location
      in
      Layout.renderTwoCol
          ( HH.div
            [ HP.classes [ Class.vcentre, Class.tight_centre ] ]
            ( HH.img [ HP.src $ pickLoc loc state.base_image]
              : Shared.renderHighlights state
              <> gameArea state
            )
            : gameControls state
          )
          ( Shared.renderNarrative unit state (videos loc) (transcript loc) (timings loc) "" HandleMessage
            <>
            Nav.renderEndButtons GoTo talkSpec.myself talkSpec.continue state
          )

    eval :: Query ~> H.ParentDSL State Query N.Query NarrativeSlot Void (M eff) 
    eval (Standard Reset next) = myReset Reset next
    eval (Standard (ResetWithBase b) next) = myReset (ResetWithBase b) next
    eval (Standard action next) = Shared.evalStandard action next
    eval (HandleMessage _ next) = Shared.evalHandleBackButton next
    eval (Game game next) = do
      H.modify (_ { game = Just game, answered = false, showAnswer = false } )
      pure next
    eval (GameAnswered correct next) = do
      if correct
        then H.modify (_ { answered = true, showAnswer = true } )
        else H.modify (_ { answered = true } )
      pure next
    eval (ShowAnswer next) = do
      H.modify (_ { answered = false, showAnswer = true } )
      pure next
    eval (ResetGame next) = do
      H.modify (_ { answered = false, showAnswer = false } )
      pure next
    eval (GoTo route next) =
      case talkSpec.continue of
        Just _  -> do
                      options <- H.gets _.options
                      extra <- Shared.evalHidden options
                      Nav.evalGoToExtra extra route next
        Nothing -> Nav.evalGoTo route next
    eval (CondAction action next) = do
      s <- H.get
      Shared.evalStandard (action s.location s.language) next

    -- Resets require an extra action in addition to the standard ones
    myReset :: forall e n.
         StdAction Query
      -> n
      -> H.ParentDSL State Query N.Query NarrativeSlot Void (M e) n
    myReset action next = do
      H.modify (_ { game = Nothing } )
      Shared.evalShowBackButton
      Shared.evalStandard action next

    videos :: Location -> Language -> N.VideoList
    videos  = Shared.videoList talkSpec.video

    transcript :: Location -> Language -> String
    transcript loc lang = pick lang $ pickLoc loc talkSpec.transcript

    timings :: Location -> Language -> Timing
    timings loc lang = pick lang $ pickLoc loc talkSpec.timings

