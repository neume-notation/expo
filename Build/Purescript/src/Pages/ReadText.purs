-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.ReadText (
    Query,
    ui
) where

import Prelude

import Data.Array                  (length, index, snoc)
import Data.Maybe                  (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Location, Language, M)
import Base.Router                 (Routes(..), Extra)
import Base.ExpoData               (Rectangle, expoData, pick)
import Base.CssClasses as Class
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T
import Pages.Shared.Activities as A

myself :: Location -> Language -> Extra -> Routes
myself = ReadText

-- The state changes are a little complex in this game:
--
--     `guessed`   is a Just value *only* when the user has *finished* the
--                 current example, either by clicking on the *correct* area,
--                 or by clicking the "give up" button.  In either case, the
--                 correct answer should be highlighted.
--     `showWrong` is Nothing except during short periods when the sad emoji
--                 should be show.
type State = A.GameState Boolean (
               showWrong  :: Maybe Boolean
             )

initialState :: T.PageState () -> State
initialState init = {
                 location:   init.location
               , language:   init.language
               , options:    init.options
               , current:    0
               , guessed:    Nothing
               , correct:    0
               , finished:   false
               , showWrong:  Nothing
               }

data Query a =   GoTo Nav.RouteSpec a
               | Resume a
               | Guess Boolean a
               | ShowAnswer a
               | ResetWrong a
               | NextOne a

type GameData = { word   :: String          -- The word to find
                , active :: Array Rectangle -- Correct places in the MS image
                }


-- The words or phrases to find, and their positions in the manuscript image,
-- are defined by `gameData`.  Note that some words appear more than once, so
-- have multiple valid answers.
--
-- The first few are complete words, but then there are a few abbreviations.
-- The index of the first abbreviation is `abbreviations`, and for examples
-- starting with this one we show an additional message very briefly
-- explaining abbreviations.
--
abbreviations :: Int
abbreviations = 5

firstDatum :: GameData
firstDatum = { word:   "quia"
             , active: [ { x: 370.0, y: 65.0,  w: 100.0, h: 70.0  }
                       , { x: 50.0,  y: 450.0, w: 100.0, h: 70.0  }
                       ]
             }

gameData :: Array GameData
gameData = [ firstDatum
           , { word:   "duodecim"
             , active: [ { x: 242.0, y: 253.0, w: 189.0, h: 70.0  } ]
             }
           , { word:   "eius"
             , active: [ { x: 468.0, y: 162.0, w: 83.0,  h: 70.0  } ]
             }
           , { word:   "dixit"
             , active: [ { x: 635.0, y: 216.0, w: 130.0, h: 70.0  } ]
             }
           , { word:   "vultis"
             , active: [ { x: 42.0,  y: 305.0, w: 130.0, h: 70.0  } ]
             }
           , { word:   "Domine"
             , active: [ { x: 185.0, y: 353.0, w: 107.0, h: 70.0  } ]
             }
           , { word:   "Christus filius dei"
             , active: [ { x: 228.0, y: 454.0, w: 211.0, h: 75.0  } ]
             }
           ] 


-- The rectangle defining the whole image - it is aligned with the image, so
-- the coordinates are zero, but the width and height depend on the actual
-- image used.
wholeImage :: Rectangle
wholeImage = { x: 0.0, y: 0.0, w: 810.0, h: 539.0 }


-- Safe indexing of gameData, returning an arbitrary default element
-- if the index is out of bounds
datumAt :: Int -> GameData
datumAt idx =
    case index gameData idx of
        Just datum -> datum
        Nothing    -> firstDatum


renderHead :: State -> H.ComponentHTML Query
renderHead state =
    let lang = state.language
    in
    HH.div [ HP.class_ HB.row ]
      [ HH.div [ HP.class_ HB.colMd10 ]
        [ HH.h2 [ HP.classes [ Class.vbigmargin, HB.textCenter ] ]
          [ HH.text $ pick lang expoData.messages.readtext_head ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.readtext_para1 ]
        , HH.img [ HP.src expoData.urls.letters, HP.class_ HB.centerBlock ]
        , HH.p [ HP.class_ Class.vseparate ]
          [ HH.text $ pick lang expoData.messages.readtext_para2 ]
        ]
      ]


-- Show the word to be found, possibly preceded by a short message about
-- abbreviations (which is always rendered, to get the layout right, but
-- made invisible when not needed)
renderWord :: State -> H.ComponentHTML Query
renderWord state =
    HH.h1 [ HP.class_ HB.textCenter ]
      [ HH.text $ (datumAt state.current) . word ]

renderQuestions :: State -> H.ComponentHTML Query
renderQuestions state =
    HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
    [ HH.div [ HP.classes [ HB.colMd8, HB.colMdOffset1 ] ]
      [ HH.div [ HP.class_ HB.well ]
        [ renderWord state
        , A.renderSummary state gameData NextOne
        ]
      ]
    ]


-- Show a final message at the end of the game
--
renderFinal :: State -> H.ComponentHTML Query
renderFinal state =
    if state.finished then
      HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
        [ HH.div [ HP.class_ HB.colMd10 ]
          [ HH.p_
            [ HH.text $ pick state.language expoData.messages.readtext_final ]
          ]
        ]
    else HH.div_ []


-- If state.guessed is Nothing (the current question is still active) then
-- make each "active" rectangle for the current word clickable, but invisible
-- of course.  Also make the entire image clickable, meaning the "wrong"
-- answer - it doesn't finish the current question, but does show feedback.
-- Otherwise (state.guessed is a Just value, meaning that the user has made
-- a correct guess, or has given up) add a border to each "active" rectangle
-- to display it, but do not make it active.
--
renderActive :: State -> Array (H.ComponentHTML Query)
renderActive state =
    let active = (datumAt state.current) . active
    in
    case state.guessed of
      Just _  -> A.showRectangle Class.border_highlight <$> active
      Nothing -> (A.activeRectangle true Guess <$> active)
                 `snoc` A.activeRectangle false Guess wholeImage


-- If a guess has been made, show a happy/sad face depending on whether it is
-- correct, and show a "give up" button unless a correct guess has been made.
--
renderControls :: State -> Array (H.ComponentHTML Query)
renderControls state =
    if state.finished then
        -- Still show a blank emoji, even though the transcript is being
        -- shown!  This is because Firefox manages to transfer the fadout
        -- to the transcript if we try to replace the emoji completely!
        A.showEmoji A.Blank Nothing
    else
        case state.guessed of
          Just ok -> if ok then A.showEmoji A.Happy Nothing
                     else A.showEmoji A.Blank Nothing
          Nothing ->
            ( case state.showWrong of
                Just start ->
                  if start then
                      -- Start the fast fade in of the sad emoji
                      A.showEmoji A.Sad (Just ResetWrong)
                  else
                      -- Fade out sad emoji
                      A.showEmoji A.SadFade Nothing
                Nothing ->
                    A.showEmoji A.Blank Nothing
            )
            <>
            A.giveUpButton state ShowAnswer


-- Show the full transcription of the text if the game is finished.
--
renderTranscript :: State -> Array (H.ComponentHTML Query)
renderTranscript state =
    if state.finished then
        [ HH.div [ HP.classes [ HB.row, Class.vbigmargin ] ]
          [ HH.div [ HP.classes [ HB.colMd6, HB.colMdOffset3 ] ]
            [ HH.p_
              [ HH.text "Propterea dixi vobis, quia nemo potest venire"
              , HH.br_
              , HH.text "ad me, nisi fuerit ei datu[m] a patre meo."
              , HH.br_
              , HH.text "Et in hoc multi discipulor[um] eius abierunt retro,"
              , HH.br_
              , HH.text "etiam non cum illo ambulaverunt.  Dixit"
              , HH.br_
              , HH.text "ergo Ih[esu]s ad duodecim: Numquid et vos"
              , HH.br_
              , HH.text "vultis abire?  Respondit ergo ei Simon"
              , HH.br_
              , HH.text "Petrus: D[omi]ne ad quem ibim[u]s? Verba vite"
              , HH.br_
              , HH.text "eterne abes.  Et nos credim[u]s et cognobim[u]s"
              , HH.br_
              , HH.text "quia tu es Christus (Xp[istu]s) filius d[e]i."
              ]
            , HH.p [ HP.classes [ HB.textRight, Class.small_text ] ]
              [ HH.text
                $ "(" <> pick state.language expoData.messages.readtext_john
                      <> " 6:66-70)"
              ]
            ]
          ]
        ]
    else []


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
      Layout.renderTwoCol
        ( [ HH.div
            [ HP.class_ Class.vbigmargin ]
            [ HH.div
              [ HP.class_ Class.tight_centre ]
              ( [ HH.img [ HP.src expoData.urls.readtext_ms ]
                ]
                <> renderActive state
              )
            ]
          ]
          <>
          renderControls state
          <>
          renderTranscript state
        )
        ( [ renderHead state
          , renderQuestions state
          , renderFinal state
          ]
          <> Nav.renderEndButtonsOr GoTo myself (Just TextMore) Resume state
        )

    eval :: Query ~> H.ComponentDSL State Query Void (M eff)
    eval (GoTo route next) = do
        -- Change to simply evalGoTo if Continue returns to the intro
        options <- H.gets _.options
        Nav.evalGoToExtra options route next
    eval (Resume next) = Nav.evalResume next
    eval (Guess ok next) = do
        -- User has clicked on the screen in an attempt to answer, `ok` shows
        -- whether it was a correct area
        H.modify (\s -> s { guessed = if ok then Just true else Nothing,
                            correct = if ok then s.correct + 1
                                      else s.correct,
                            finished = ok && s.current == length gameData - 1,
                            showWrong = if ok then Nothing else Just true
                          }
                 )
        pure next
    eval (ShowAnswer next) = do
        -- Explicit "give up"
        H.modify (\s -> s { guessed = Just false,
                            finished = s.current == length gameData - 1,
                            showWrong = Nothing
                          }
                 )
        pure next
    eval (ResetWrong next) = do
        -- The `showWrong` flag in the state is used purely to cause the
        -- sad emoji to show.  It is shown using a fast fade-in transition,
        -- while also setting a handler for the end of the transtion.  The
        -- handler calls this ResetGame action, and is the only way this
        -- action is invoked.  Here we set `showWrong` false, which starts
        -- a slow fade out of the emoji.
        H.modify (_ { showWrong = Just false } )
        pure next
    eval (NextOne next) = do
        H.modify (\s -> s { current = s.current + 1,
                            guessed = Nothing,
                            showWrong = Nothing
                          }
                 )
        pure next
