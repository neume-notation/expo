-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Shared.Activities (
    GameState,
    renderSummary,
    Emoji(..),
    showEmoji,
    giveUpButton,
    activeRectangle,
    showRectangle
) where

-- Shared utilities for games
--
-- `GameState` and `renderSummary` are specific to sequential question/answer
-- games, that is, games with a list of questions, shown one at a time,
-- which the user has to answer (by a game-dependent mechanism) before
-- being allowed to move on to the next question.
--
-- `showEmoji`, `giveUpButton` and `activeRectangle` do not depend on the
-- form of GameState, so have wider applicability.

import Prelude

import Data.Array                  ((:), length)
import Data.Maybe                  (Maybe(..))

import CSS.Geometry as Geometry
import CSS.Size as Size
import Halogen as H
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.ExpoData               (Rectangle, expoData, pick)
import Base.CssClasses as Class
import Pages.Shared.Types as T


-- Sequential games have these standard fields in the state:
--
--     current   The index in the sequence of questions
--     guessed   Whether the current question has been attempted, and if so
--               with what result/answer
--     correct   The running total of correct answers
--     finished  Whether the game is finished (the final question has been
--               answered - a convenience as it can be deduced from `current`,
--               `guessed` and the number of available questions).
--
type GameState a r = T.PageState (
                       current    :: Int
                     , guessed    :: Maybe a
                     , correct    :: Int
                     , finished   :: Boolean
                     | r
                     )


-- If at the end, show the score (possibly with a "well done" sort of message,
-- maybe depending on the score, and maybe some puns, like the BBC quizzes?)
-- Otherwise, show a running score and the number of remaining questions, and
-- if the current one has been guessed, show a "Next" button.
--
-- `nextOne` is an action in the specific game's query algebra, and it should
-- move the game on to the next question.
--
-- It is assumed that this is placed in a container whose full content width
-- is to be used
--
renderSummary :: forall f a r g p.
                    GameState a r
                 -> Array g
                 -> H.Action f
                 -> H.HTML p f
renderSummary state gameData nextOne =
    HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
      ( if state.finished
        then endScore
        else runningScore <> moreQuestions <> nextButton
      )
  where
    endScore =
      [ HH.div [ HP.class_ HB.colMd12 ]
        [ HH.h3 [ HP.class_ HB.textCenter ]
          [ HH.text $ pick state.language expoData.messages.game_score ]
        , HH.h1 [ HP.class_ HB.textCenter ]
          [ HH.text $ show state.correct <> "/" <> show (length gameData) ]
        ]
      ]
    runningScore =
      [ HH.div [ HP.class_ HB.colMd4 ]
        [ HH.p_
          [ HH.text
            $ pick state.language expoData.messages.game_score
              <> ": " <> show state.correct
          ]
        ]
      ]
    moreQuestions =
      let more = length gameData - 1 - state.current
      in
      if more > 0
      then [ HH.div [ HP.class_ HB.colMd4 ]
             [ HH.p_
               [ HH.text
                 $ show (length gameData - 1 - state.current)
                   <> " "
                   <> ( if more > 1
                        then pick state.language expoData.messages.game_more
                        else pick state.language expoData.messages.game_penult
                      )
               ]
             ]
           ]
      else []
    nextButton =
      case state.guessed of
        Just _ ->
          [ HH.div [ HP.class_ HB.colMd4 ]
            [ HH.button
              [ HP.classes [ Class.grey_button
                           , Class.small_long_button
                           , HB.pullRight
                           ]
              , HE.onClick $ HE.input_ nextOne
              ]
              [ HH.text $ pick state.language expoData.messages.game_next ]
            ]
          ]
        Nothing -> []


data Emoji = Blank | Happy | Sad | SadFade


-- The `why` parameter determines what is shown:
--
--     Blank    Don't show anything (but in a subtle way - see below)
--     Happy    Show a happy face
--     Sad      Show a sad face, but do so with a fast transition, setting
--              a handler onTransitionEnd to invoke `resetAction`
--     SadFade  Clear a sad emoji with a slow transition; the idea is that
--              this state will be shown as a result of `resetAction`, so
--              that sad emojis are only shown for a limited period.
--
-- The Blank case does still show the div and an image, but hidden using
-- the .faded class specifically: this sets the opacity to zero, which
-- allows a .fadein to work going from the Blank state to the Sad state.
-- If we don't start from an opacity different from one, there is nothing
-- to transtion, and we don't fire the transitionEnd event.  The .faded
-- class also sets `transition-property: none`, which seems to be needed by
-- Firefox (but not Chrome) to cancel an existing running fadeout transition.
--
-- The layout is specific to the uses required, where positioning is fixed
-- relative to the bottom of a full height container.
--
showEmoji :: forall f p. Emoji -> Maybe (H.Action f) -> Array (H.HTML p f)
showEmoji why resetAction =
    let handler = case resetAction of
                      Just act -> [ HE.onTransitionEnd $ HE.input_ act ]
                      Nothing  -> []
    in
    case why of
        Blank   -> doEmoji expoData.urls.emoji_sad   [ Class.faded ] []
        Happy   -> doEmoji expoData.urls.emoji_happy [] []
        Sad     -> doEmoji expoData.urls.emoji_sad   [ Class.fadein ] handler
        SadFade -> doEmoji expoData.urls.emoji_sad   [ Class.fadeout ] []
  where
    doEmoji url fadeclass fadeevent =
      [ HH.div [ HP.class_ Class.at_bottom
               , CSS.style $ Geometry.bottom (Size.px 200.0)
               ]
           [ HH.div
               (HP.classes (Class.tight_centre : fadeclass) : fadeevent)
               [ HH.img [ HP.src url
                        , HP.width 100
                        , HP.height 100
                        ]
               ]
           ]
      ]


-- The `giveUpAction` should be an action in the query algebra whose purpose
-- is to display the correct answer.
--
-- The layout is specific to the uses required, where positioning is fixed
-- relative to the bottom of a full height container.
--
giveUpButton :: forall f r p. T.PageState r -> H.Action f -> Array (H.HTML p f)
giveUpButton state giveUpAction =
    [ HH.div [ HP.class_ Class.at_bottom]
      [ HH.div [ HP.class_ Class.tight_centre ]
        [ HH.button
          [ HE.onClick $ HE.input_ giveUpAction
          , HP.classes [ Class.dark_green_button, Class.long_button ]
          ]
          [ HH.text $ pick state.language expoData.messages.give_up ]
        ]
      ]
    ]


-- Make a rectangular area clickable.
--
-- `answerAction` should be a function producing an action in the query
-- algebra.  Its parameter is a boolean showing whether the answer is a
-- correct one or not.
--
-- `isCorrect` is passed back as the parameter to `answerAction`, and
-- it is also used to determine the z-order: areas for correct answers
-- have precedence over those for incorrect answers, because the latter is
-- likely to be the "rest of the image".
--
activeRectangle :: forall f p.
                      Boolean
                   -> (Boolean -> H.Action f)
                   -> Rectangle
                   -> H.HTML p f
activeRectangle isCorrect answerAction rectangle =
    HH.div
      [ HE.onClick $ HE.input_ (answerAction isCorrect)
      , HP.classes [ if isCorrect then Class.must_be_top else Class.as_mask
                   , Class.touchclick
                   ]
      , cssRectangle rectangle
      ]
      []


-- Show a rectangle, typically to highlight a rectangular area by making
-- a div with a border.  The class passed in is what defines the effect,
-- since otherwise this is just a blank positioned div of the right size.
--
showRectangle :: forall f p. HH.ClassName -> Rectangle ->  H.HTML p f
showRectangle effectClass rectangle =
    HH.div
      [ HP.classes [ Class.as_mask, effectClass ]
      , cssRectangle rectangle
      ]
      []


-- Internal helper for both of the previous two functions
--
cssRectangle :: forall i r. Rectangle -> HP.IProp (style :: String | r) i
cssRectangle rectangle =
    CSS.style do Geometry.left (Size.px rectangle.x)
                 Geometry.top (Size.px rectangle.y)
                 Geometry.width (Size.px rectangle.w)
                 Geometry.height (Size.px rectangle.h)
