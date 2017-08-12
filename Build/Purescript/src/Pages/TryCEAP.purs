-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.TryCEAP (
    Query,
    ui
) where

import Prelude

import Data.Array                  ((:), length, zipWith, elem, find,
                                    head, last, drop, concat, mapWithIndex)
import Data.Foldable               (product, maximum)
import Data.Int                    (floor)
import Data.Ord                    (abs)
import Data.Unfoldable as U

import Control.Monad.Eff           (Eff)
import Data.Maybe                  (Maybe(..), fromMaybe)
import DOM                         (DOM())
import DOM.HTML.Types              (HTMLElement)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Location, Language, M)
import Base.Router                 (Routes(..), Extra)
import Base.ExpoData               (Message, expoData, pick, ChantSyl)
import Base.CssClasses as Class
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T

myself :: Location -> Language -> Extra -> Routes
myself = TryCEAP

-- An opaque type for references to the EditNeumes object
foreign import data EditNeumes :: Type

foreign import createCEAP ::
    forall eff. HTMLElement -> String -> Eff (dom :: DOM | eff) EditNeumes

foreign import destroyCEAP ::
    forall eff. EditNeumes -> Eff (dom :: DOM | eff) Unit

foreign import getChantData ::
    forall eff. EditNeumes -> Eff (dom :: DOM | eff) (Array ChantSyl)

foreign import markBadBoxes ::
    forall eff. EditNeumes -> Array Int -> Eff (dom :: DOM | eff) Unit


type State = T.PageState (
               ceap       :: Maybe EditNeumes
             , score      :: Maybe Scores
             )

initialState :: T.PageState () -> State
initialState init = {
                 location: init.location
               , language: init.language
               , options:  init.options
               , ceap:     Nothing
               , score:    Nothing
               }

data Query a =   Initialize a
               | Finalize a
               | GoTo Nav.RouteSpec a
               | Resume a
               | Mark a


-- Mark the first chant transcription against the second (which is expected
-- to be the "official" one.  It is rather heuristic!
--
-- The score consists of four separate values between 0.0 and 1.0, which
-- are each calculated by multiplying factors together for each error found.
-- The four scores are:
--
--     nonBlank      1.0 * blankFactor for each syllable left totally blank
--     shapeOK       1.0 * a factor for each neume which does not belong in
--                         a syllable - one of two factors is used in each
--                         case depending on whether or not the shapes are
--                         "similar" (similarFactor and notSimilarFactor)
--     countOK       1.0 * a factor for each syllable with the wrong
--                         number of neumes (wrongCountFactor)
--     positionOK    1.0 * a factor for each neume out of position compared
--                         with its predecessor.  This is the most vague,
--                         as it depends on the correct neume selection, too
--                         Two factors are used depending on whether the error
--                         is "small" or "large".
--
-- A single score can be produced from these as:
--
--     nonBlank ( 3.0 + countOK ( 2.0 + shapeOK ( 3.0 + positionOK * 2.0)))
--
-- Which has the effect that trying something in each syllable gives
-- at least 3.0, putting the correct number of neumes in each syllable
-- gives at least 5.0, and so on.
--
-- For each class of score, we keep the per-syllable scores as well as the
-- overall result, so that the syllables with mistakes can be highlighted.
--
type SylScore = { s :: Number, all :: Array Number }

type Scores = { nonBlank   :: SylScore
              , countOK    :: SylScore
              , shapeOK    :: SylScore
              , positionOK :: SylScore
              }

blankFactor :: Number
blankFactor = 0.85
wrongCountFactor :: Number
wrongCountFactor = 0.9
similarFactor :: Number
similarFactor = 0.98
notSimilarFactor :: Number
notSimilarFactor = 0.9
nearPosFactor :: Number
nearPosFactor = 0.98
farPosFactor :: Number
farPosFactor = 0.9
nearOffset :: Int  -- within which there is no penalty
nearOffset = 10
farOffset :: Int   -- within which only nearPosFactor
farOffset = 20

markThis :: Array ChantSyl -> Array ChantSyl -> Scores
markThis chant model =
    { nonBlank:
          let countBlank c = if length c.neumes > 0 then 1.0 else blankFactor
              perSyl       = countBlank <$> chant
          in { s: product perSyl, all: perSyl }
    , countOK:
          let cnt c      = length c.neumes
              cc         = cnt <$> chant
              cm         = cnt <$> model
              check  c m = if c > 0 && c /= m then wrongCountFactor else 1.0
                           -- Don't count error again for blanks
              perSyl     = zipWith check cc cm
          in { s: product perSyl, all: perSyl }
    , shapeOK:
          let similar m = case find (m `elem` _) expoData.miniceap.similar of
                            Just sim -> sim
                            Nothing  -> []
              shape n m = if n == m then 1.0
                          else if n `elem` similar m then similarFactor
                          else notSimilarFactor
              nids c    = (_.neume) <$> c
              nval nc m = -- For one neume `nc` in `chant`, find the maximum
                          -- of comparing neumes by `shape` with all neumes on
                          -- the syllable in `model`.  In other words, ignore
                          -- the order (position) and duplicates, both of which
                          -- are handled by other score components
                          fromMaybe 1.0 $ maximum (shape nc <$> nids m.neumes) 
              syl c m   = -- For a syllable of `chant`, score it by mapping
                          -- `nval` over its neumes and taking the product.
                          product (flip nval m <$> nids c.neumes)
              perSyl    = zipWith syl chant model
          in { s: product perSyl, all: perSyl }
    , positionOK:
          let
              pos n m   = -- position relationship between two neumes
                          { x: m.x - n.x, y: m.y - n.y }
              syl s     = -- position relationships within one syllable
                          zipWith pos s.neumes (drop 1 s.neumes)
              inter s t = -- position (height only) between one syllable and the
                          -- next.  Assumes neumes are ordered by 'x' position
                          let sl = U.fromMaybe $ last s.neumes
                              tf = U.fromMaybe $ head t.neumes
                              posy n m = { x: 0, y: m.y - n.y }
                          in zipWith posy sl tf
              posall c  = -- All position relationships for one chant: the
                          -- inter-syllable height is prepended onto the
                          -- list of relationships within the syllable for
                          -- each syllable after the first
                          let dummysyl = { ident: 0, txt: "", neumes: [] }
                              doIt prev cur = inter prev cur <> syl cur
                          in zipWith doIt (dummysyl : c) c
              scor1 p q = -- Score a single pair of position relationships,
                          -- presumably one of each chant
                          let dx = abs $ p.x - q.x
                              dy = abs $ p.y - q.y
                              d  = max dx dy
                          in if d > farOffset then farPosFactor
                             else if d > nearOffset then nearPosFactor
                             else 1.0 
              score c m = -- Score positions for one syllable against those
                          -- of the model
                          product (zipWith scor1 c m)
              perSyl    = zipWith score (posall chant) (posall model)
          in { s: product perSyl, all: perSyl }
    }


-- A single score as an integer out of 10
--
combineScores :: Scores -> Int
combineScores scores =
      -- The total available for getting the shape right is slighly increased
      -- to make it easier to get full marks after the `floor`
      floor $ scores.nonBlank.s * (
                  3.0 + scores.countOK.s * (
                      2.0 + scores.shapeOK.s * (
                          3.3 + scores.positionOK.s * 2.0
                      )
                  )
              )


-- The list of boxes in which the score of one type is less than perfect.
-- The integer values are the indices, which is safe since the foreign code
-- for `getChantData` ensures that all indices are used, and therefore
-- can be directly matched to `ident` values.
--
badBoxes :: Array Number -> Array Int
badBoxes sylScores = concat (mapWithIndex eachSyl sylScores)
  where
    top = 0.999  -- A little less than 1.0 in case there is any rounding
    eachSyl i s = if s < top then [i]
                  else []


-- Hint about the most important thing to try improving.
-- The choice between hinting about shapes and hinting about position is made
-- on the basis of which one is having the bigger effect on the combined
-- score.
--
improvementHint :: Scores -> { msg :: Message, bad :: Array Int }
improvementHint scores =
    let top = 0.999  -- A little less than 1.0 in case there is any rounding
        posWeight = 2.0 / (3.3 + 2.0)
                     -- Relative importance of position vs shape in total score
    in
    if combineScores scores == 10 then
        { msg: expoData.messages.tryceap_good , bad: [] }
    else if scores.nonBlank.s < top then
        { msg: expoData.messages.tryceap_blanks
        , bad: badBoxes scores.nonBlank.all
        }
    else if scores.countOK.s < top then
        { msg: expoData.messages.tryceap_count
        , bad: badBoxes scores.countOK.all
        }
    else if scores.shapeOK.s < 1.0 - (1.0 - scores.positionOK.s) * posWeight
      then
        { msg: expoData.messages.tryceap_shape
        , bad: badBoxes scores.shapeOK.all
        }
    else
        { msg: expoData.messages.tryceap_pos
        , bad: badBoxes scores.positionOK.all
        }


renderHead :: State -> H.ComponentHTML Query
renderHead state =
    let lang = state.language
    in
    HH.div [ HP.class_ HB.row ]
      [ HH.div [ HP.class_ HB.colMd10 ]
        [ HH.h2 [ HP.classes [ Class.vbigmargin, HB.textCenter ] ]
          [ HH.text $ pick lang expoData.messages.tryceap_head ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.tryceap_para1 ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.tryceap_para2 ]
        ]
      ]


renderScore :: State -> H.ComponentHTML Query
renderScore state =
    let lang = state.language
    in
    case state.score of
      Just s  -> let total = combineScores s
                 in
                 HH.div_
                 [ HH.h1 [ HP.class_ HB.textCenter ]
                   [ HH.text $ pick lang expoData.messages.game_score
                               <> ": " <> show total <> "/10"
                   ]
                 , HH.p_ [ HH.text $ pick lang ((improvementHint s) . msg)
                           <>
                           if total < 10 then
                               " " <> pick lang expoData.messages.tryceap_again
                           else ""
                         ]
                 ]
      Nothing -> HH.div_
                 [ HH.p_
                   [ HH.text $ pick lang expoData.messages.tryceap_init ]
                 ]


renderFeedback :: State -> H.ComponentHTML Query
renderFeedback state =
    HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
    [ HH.div [ HP.class_ HB.colMd10 ]
      [ HH.div [ HP.class_ HB.well ]
        [ renderScore state
        , HH.div [ HP.class_ Class.tight_centre ]
          [ HH.button
            [ HP.classes [ Class.grey_button
                         , Class.small_long_button
                         , Class.vseparate
                         ]
            , HE.onClick $ HE.input_ Mark
            ]
            [ HH.text $ pick state.language expoData.messages.game_score ]
          ]
        ]
      ]
    ]


ui :: forall eff. H.Component HH.HTML Query (T.PageState ()) Void (M eff)
ui = H.lifecycleComponent { initialState: initialState
                          , render
                          , eval
                          , receiver: const Nothing
                          , initializer: Just (H.action Initialize)
                          , finalizer: Just (H.action Finalize)
                          }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      Layout.renderTwoCol
        [ HH.div [ HP.class_ Class.vseparate ]
          [ HH.div [ HP.class_ Class.tight_centre ]
            [ HH.img [ HP.src expoData.urls.ceap_in_hoc ] ]
          ]
        , HH.div
            -- This is where we attach the jQuery miniceap widget
            [ HP.class_ Class.vseparate
            , HP.ref $ H.RefLabel "miniceap"
            ]
            []
        ]
        ( [ renderHead state
          , renderFeedback state
          ]
          <> Nav.renderEndButtonsOr GoTo myself Nothing Resume state
        )

    eval :: Query ~> H.ComponentDSL State Query Void (M eff)
    eval (Initialize next) = do
        at <- H.getHTMLElementRef (H.RefLabel "miniceap")
        case at of
          Just dom -> do
            e <- H.liftEff $ createCEAP dom expoData.miniceap.html
            H.modify (_ { ceap = Just e } )
          Nothing -> pure unit
        pure next
    eval (Finalize next) = do
        c <- H.gets _.ceap
        H.modify (_ { ceap = Nothing } )          -- Remove object reference
        case c of
          Just c' -> H.liftEff $ destroyCEAP c'   -- Remove from DOM / cleanup
          Nothing -> pure unit
        pure next
    -- Change to evalGoToExtra if a Continue button is used, as we would
    -- need to pass on the returnTo parameter
    eval (GoTo route next) = Nav.evalGoTo route next
    eval (Resume next) = Nav.evalResume next
    eval (Mark next) = do
        c <- H.gets _.ceap
        case c of
          Just c' -> do
                       chant <- H.liftEff $ getChantData c'
                       let scores = markThis chant expoData.miniceap.transcript
                           boxes  = (improvementHint scores) . bad
                       H.liftEff $ markBadBoxes c' boxes
                       H.modify (_ { score = Just scores } )
                       pure unit
          Nothing -> pure unit
        pure next
