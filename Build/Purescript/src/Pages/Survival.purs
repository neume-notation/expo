-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Survival (
    Query,
    ui
) where

import Prelude

import Data.Array                  (snoc, take, drop, reverse, index, length)
import Data.Maybe                  (Maybe(..))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Location, Language, M)
import Base.Router                 (Routes(..), Extra)
import Base.ExpoData               (Message, expoData, pick)
import Base.CssClasses as Class
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T
import Pages.Shared.Activities as A

myself :: Location -> Language -> Extra -> Routes
myself = Survival

type State = A.GameState Int ()

initialState :: T.PageState () -> State
initialState init = {
                 location: init.location
               , language: init.language
               , options:  init.options
               , current:  0
               , guessed:  Nothing
               , correct:  0
               , finished: false
               }

data Query a =   GoTo Nav.RouteSpec a
               | Resume a
               | Guess Int a
               | NextOne a

type GameData = { ms     :: String    -- URL of manuscript image from expoData
                , right  :: Int       -- Index in gameOptions of the answer
                , decoys :: Array Int -- Additional wrong options to show
                , answer :: Message   -- Full description for this case
                }

-- All of the possible options for answer choices.  They are not all shown
-- for each manuscript - see the `right` and `decoys` fields of GameData.
--
firstOption :: Message
firstOption = expoData.messages.survive_opt0
gameOptions :: Array Message
gameOptions = [
    firstOption
  , expoData.messages.survive_opt1
  , expoData.messages.survive_opt2
  , expoData.messages.survive_opt3
  , expoData.messages.survive_opt4
  , expoData.messages.survive_opt5
  ]

-- In practice this exactly parallels gameOptions, but in principle entries
-- here could share the same "right" answer in gameOptions, and, even if they
-- did, might have different detailed "answer" descriptions.
--
firstDatum :: GameData
firstDatum = {
      ms:     expoData.urls.survive_ms0
    , right:  0
    , decoys: [4, 5]
    , answer: expoData.messages.survive_ans0
    }
gameData :: Array GameData
gameData = [
    firstDatum
  , { ms:     expoData.urls.survive_ms1
    , right:  1
    , decoys: [2, 3]
    , answer: expoData.messages.survive_ans1
    }
  , { ms:     expoData.urls.survive_ms2
    , right:  2
    , decoys: [0, 4]
    , answer: expoData.messages.survive_ans2
    }
  , { ms:     expoData.urls.survive_ms3
    , right:  3
    , decoys: [1, 5]
    , answer: expoData.messages.survive_ans3
    }
  , { ms:     expoData.urls.survive_ms4
    , right:  4
    , decoys: [0, 2]
    , answer: expoData.messages.survive_ans4
    }
  , { ms:     expoData.urls.survive_ms5
    , right:  5
    , decoys: [1, 3]
    , answer: expoData.messages.survive_ans5
    }
  ]

-- Safe indexing of the above arrays, returning an arbitrary default element
-- if the index is out of bounds
optionAt :: Int -> Message
optionAt idx =
    case index gameOptions idx of
        Just option -> option
        Nothing     -> firstOption

datumAt :: Int -> GameData
datumAt idx =
    case index gameData idx of
        Just datum -> datum
        Nothing    -> firstDatum

-- This always gives a permutation, but, in addtion, if the array has
-- three elements, it covers all possible permutations as `n` goes through
-- all the available values mod 6.
permute :: forall a. Int -> Array a -> Array a
permute n arr =
    let firstPos = n `mod` 3
        first    = take 1 $ drop firstPos arr
        restRaw  = take firstPos arr <> drop (firstPos + 1) arr
        rest     = if (n `mod` 2 == 0) then restRaw
                   else reverse restRaw
    in
    first <> rest


renderHead :: State -> H.ComponentHTML Query
renderHead state =
    let lang = state.language
    in
    HH.div [ HP.class_ HB.row ]
      [ HH.div [ HP.class_ HB.colMd10 ]
        [ HH.h2 [ HP.classes [ Class.vbigmargin, HB.textCenter ] ]
          [ HH.text $ pick lang expoData.messages.survive_head ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.survive_para1 ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.survive_para2 ]
        ]
      ]


-- This is the area in which the user makes their guesses and gets feedback.
-- If state.guessed is Nothing
--   - Show a list of options - include "radio" buttons or similar images,
--     but clicking anywhere in the whole option works.  Options are the
--    "right" answer plus the "decoys" specified in the gameData, put in
--     an order determined by a pseudo-random-ish formula based on
--     the sequence number (`state.current`)
-- Else
--   - Still show the options, no longer active, and with the selected one
--     highlighted
--   - Show whether the last guess was right or wrong
--   - Show the full descriptive answer
--
renderGuess :: State -> H.ComponentHTML Query
renderGuess state =
    let datum = datumAt state.current
        order = permute state.current (snoc datum.decoys datum.right)
    in
    HH.div [ HP.class_ Class.vseparate ]
      [ HH.div [ HP.class_ HB.row ]
        [ HH.div [ HP.class_ HB.colMd12 ]
          [ HH.table [ HP.classes [ HB.table, HB.tableBordered ] ]
            [ HH.tbody_ (showOpt state.guessed datum.right <$> order) ]
          ]
        ]
      , HH.div [ HP.class_ HB.row ]
        [ HH.div [ HP.class_ HB.colMd12 ]
          $ descriptive state.guessed datum.right datum.answer
        ]
      ]
  where
    showOpt guess answer opt =
        let props =
                case guess of
                  Nothing -> [ HE.onClick $ HE.input_ (Guess opt) ]
                  Just g  -> if g == opt then [HP.class_ HB.active]
                             else []
            radio =
                case guess of
                  Nothing -> expoData.urls.radio_off
                  Just g  -> if g == opt then expoData.urls.radio_on
                             else expoData.urls.radio_off
        in
        HH.tr props
          [ HH.td_
            [ HH.img [ HP.src radio ]
            , HH.span_ [ HH.text $ pick state.language $ optionAt opt ]
            ]
          ]
    descriptive guess answer desc=
        case guess of
          Nothing -> []
          Just g  ->
              [ HH.p_
                [ HH.text $
                    ( if g == answer
                      then pick state.language expoData.messages.game_right
                      else pick state.language expoData.messages.game_wrong
                    )
                    <> " "
                    <> pick state.language desc
                ]
              ]


renderQuestions :: State -> H.ComponentHTML Query
renderQuestions state =
    HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
    -- [ HH.div [ HP.classes [ HB.colMd8, HB.colMdOffset1 ] ]
    [ HH.div [ HP.class_ HB.colMd10 ]
      [ HH.div [ HP.class_ HB.well ]
        [ renderGuess state
        , A.renderSummary state gameData NextOne
        ]
      ]
    -- , HH.div [ HP.class_ HB.colMd3 ]
    , HH.div [ HP.class_ HB.colMd2 ]
      $ smiley state.guessed (datumAt state.current ) . right
    ]
  where
    smiley guess answer =
        case guess of
          Nothing -> []
          Just g  ->
              [ HH.div [ HP.class_ Class.tight_centre ]
                [ HH.img
                  [ HP.src $ if g == answer
                             then expoData.urls.emoji_happy
                             else expoData.urls.emoji_sad
                  , HP.width 100
                  , HP.height 100
                  ]
                ]
              ]


ui :: forall eff. H.Component HH.HTML Query (T.PageState ()) Void (M eff)
ui = H.component {
       initialState: initialState
     , render
     , eval
     , receiver: const Nothing
     }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      -- Image on the left
      Layout.renderTwoCol
        [ HH.div
          [ HP.classes [ Class.vcentre, Class.tight_centre ] ]
          [ HH.img [ HP.src $ (datumAt state.current) . ms ] ]
        ]
        ( [ renderHead state
          , renderQuestions state
          ]
          <> Nav.renderEndButtonsOr GoTo myself Nothing Resume state
        )

    eval :: Query ~> H.ComponentDSL State Query Void (M eff)
    -- Change to evalGoToExtra if Continue is not Nothing, as we would
    -- need to pass on the returnTo parameter
    eval (GoTo route next) = Nav.evalGoTo route next
    eval (Resume next) = Nav.evalResume next
    eval (Guess guess next) = do
        H.modify (\s -> s { guessed = Just guess,
                            correct = if guess == (datumAt s.current) . right
                                      then s.correct + 1
                                      else s.correct,
                            finished = s.current == length gameData - 1
                          }
                 )
        pure next
    eval (NextOne next) = do
        H.modify (\s -> s { current = s.current + 1, guessed = Nothing } )
        pure next
