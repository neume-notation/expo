-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Phrases (
    Query,
    ui
) where

import Prelude

import Data.Array                  ((..), length, index, elem,
                                    snoc, zipWith, difference)
import Data.Maybe                  (Maybe(..))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Location, Language, M)
import Base.Router                 (Routes(..), Extra)
import Base.ExpoData               (Message, Rectangle, expoData, pick)
import Base.CssClasses as Class
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T
import Pages.Shared.Activities as A

myself :: Location -> Language -> Extra -> Routes
myself = Phrases

type State = T.PageState (
               thisOne :: Maybe Int     -- Last one clicked, if any
             , found   :: Array Int     -- All clicked so far
             )

initialState :: T.PageState () -> State
initialState init = {
                 location: init.location
               , language: init.language
               , options:  init.options
               , thisOne:  Nothing
               , found:    []
               }

data Query a =   GoTo Nav.RouteSpec a
               | Resume a
               | Clicked Int a
               | ShowMe a

type GameData = { desc   :: Message     -- Description why it is a break
                , active :: Rectangle   -- Clickable area
                , marker :: Rectangle   -- Marker for feedback
                }


-- The active areas at the ends of lines (3 and 8) are wider because of
-- there is no point restricting them much, and in the big gap after
-- "querella" a bigger one is used.  The active areas also extend further
-- upwards into the music than the markers.  These are the reasons why the
-- `marker` areas are separately defined.  The markers are centred vertically
-- on lines midway beween the top of the modern non-ascending letters and the
-- approximate baseline of the manuscript letters - this works out as 34px
-- above the baseline of the modern text in the image, which gives us
-- y-coordinates 120, 264, 406 and 554.  The active areas are centred
-- vertically 25px higher than the markers, to include more of the music,
-- but horizontally the non-line-end ones are centred the same way as the
-- markers.
gameData :: Array GameData
gameData = [ { desc:   expoData.messages.phrases_expl1
             , active: { x: 278.0, y: 30.0, w:  54.0,  h: 150.0 }
             , marker: { x: 290.0, y: 95.0, w:  30.0,  h: 50.0  }
             -- centre (305,120)
             }
           , { desc:   expoData.messages.phrases_expl2
             , active: { x: 521.0, y: 30.0,  w: 54.0,  h: 150.0 }
             , marker: { x: 533.0, y: 95.0,  w: 30.0,  h: 50.0  }
             -- centre (548,120)
             }
           , { desc:   expoData.messages.phrases_expl3
             , active: { x: 728.0, y: 30.0,  w: 82.0,  h: 150.0 }
             , marker: { x: 740.0, y: 95.0,  w: 30.0,  h: 50.0  }
             -- centre (755,120) - extend active right to image RHS
             }
           , { desc:   expoData.messages.phrases_expl4
             , active: { x: 490.0, y: 174.0, w: 54.0,  h: 150.0 }
             , marker: { x: 502.0, y: 239.0, w: 30.0,  h: 50.0  }
             -- centre (517,264)
             }
           , { desc:   expoData.messages.phrases_expl2
             , active: { x: 61.0,  y: 316.0, w: 54.0,  h: 150.0 }
             , marker: { x: 73.0,  y: 381.0, w: 30.0,  h: 50.0  }
             -- centre (88,406)
             }
           , { desc:   expoData.messages.phrases_expl2
             , active: { x: 294.0, y: 316.0, w: 84.0,  h: 150.0 }
             , marker: { x: 321.0, y: 381.0, w: 30.0,  h: 50.0  }
             -- centre (336,406) - extend active since word gap is wide
             }
           , { desc:   expoData.messages.phrases_expl7
             , active: { x: 42.0,  y: 464.0, w: 54.0,  h: 150.0 }
             , marker: { x: 54.0,  y: 529.0, w: 30.0,  h: 50.0  }
             -- centre (69,554)
             }
           , { desc:   expoData.messages.phrases_expl8
             , active: { x: 463.0, y: 464.0, w: 134.0, h: 150.0 }
             , marker: { x: 485.0, y: 529.0, w: 30.0,  h: 50.0  }
             -- centre (500,554) - extend active 10px left and 70px right
             }
           ]


renderHead :: State -> H.ComponentHTML Query
renderHead state =
    let lang = state.language
    in
    HH.div [ HP.class_ HB.row ]
      [ HH.div [ HP.class_ HB.colMd10 ]
        [ HH.h2 [ HP.classes [ Class.vbigmargin, HB.textCenter ] ]
          [ HH.text $ pick lang expoData.messages.phrases_head ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.phrases_para1 ]
        , HH.p_ [ HH.text $ pick lang expoData.messages.phrases_para2 ]
        ]
      ]


renderFeedback :: State -> H.ComponentHTML Query
renderFeedback state =
    let lang    = state.language
        remain  = length gameData - length state.found
        toGoMsg = if remain > 0 then
                      show remain <> " "
                      <> pick lang expoData.messages.phrases_more
                  else
                      pick lang expoData.messages.phrases_done
        explain = (\g -> g.desc) <$> (state.thisOne >>= index gameData)
    in
    HH.div [ HP.classes [ HB.row, Class.vseparate ] ]
    [ HH.div [ HP.class_ HB.colMd10 ]
      [ HH.div [ HP.class_ HB.well ]
        [ case explain of
            Just e  -> HH.p_ [ HH.text $ pick lang e ]
            Nothing -> HH.div_ []
        , HH.h1 [ HP.class_ HB.textCenter ]
          [ HH.text toGoMsg ]
        , if remain > 0 then 
              HH.div [ HP.class_ Class.tight_centre ]
              [ HH.button
                [ HP.classes [ Class.grey_button
                             , Class.small_long_button
                             , Class.vseparate
                             ]
                , HE.onClick $ HE.input_ ShowMe
                ]
                [ HH.text $ pick state.language expoData.messages.show_me ]
              ]
          else
              -- Tweak the layout (a bit of a hack)
              HH.div_ [ HH.text "\x00a0" ]
        ]
      ]
    ]


renderActive :: State -> Array (H.ComponentHTML Query)
renderActive state =
    zipWith doActive (0 .. (length gameData - 1)) gameData
  where
    doActive idx datum =
        A.activeRectangle true (const $ Clicked idx) datum.active


renderMarkers :: State -> Array (H.ComponentHTML Query)
renderMarkers state =
    doMarker state.thisOne <$> state.found
  where
    doMarker markCurrent mark =
        let effect = if markCurrent == Just mark then HH.ClassName "diamond-red"
                     else HH.ClassName "diamond-blue"
        in
        case index gameData mark of
          Just datum -> A.showRectangle effect datum.marker
          Nothing    -> HH.div_ []


-- Show the translation of the latin, in a block which is centred relative
-- to its parent.
--
-- Markers are added to the text by changing the background colour of space
-- characters.  To avoid any possible width discrepancy in fonts (there are
-- reports of some) a non-breaking space is used only at the ends of lines.
--
renderTranslation :: State -> H.ComponentHTML Query
renderTranslation state =
    let lang = state.language
        red  = HH.ClassName "text-mark-red"
        blue = HH.ClassName "text-mark-blue"
        divide n lineEnd =
            let space = if lineEnd then "\x00a0" else " "
            in if elem n state.found then
                   HH.span
                     [ HP.class_ if state.thisOne == Just n then red else blue ]
                     [ HH.text space ]
               else HH.text space
    in
    HH.div [ HP.class_ Class.tight_centre ]
      [ HH.p [ HP.class_ Class.vbigmargin ]
        [ HH.text $ pick lang expoData.messages.phrases_trans1
        , divide 0 false
        , HH.text $ pick lang expoData.messages.phrases_trans2
        , divide 1 false
        , HH.text $ pick lang expoData.messages.phrases_trans3
        , divide 2 true
        , HH.br_
        , HH.text $ pick lang expoData.messages.phrases_trans4
        , divide 3 false
        , HH.text $ pick lang expoData.messages.phrases_trans5
        , divide 4 true
        , HH.br_
        , HH.text $ pick lang expoData.messages.phrases_trans6
        , divide 5 false
        , HH.text $ pick lang expoData.messages.phrases_trans7
        , divide 6 true
        , HH.br_
        , HH.text $ pick lang expoData.messages.phrases_trans8
        , divide 7 true
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
      Layout.renderTwoCol
        [ HH.div
          [ HP.classes [ Class.vcentre, Class.tight_centre ] ]
          ( [ HH.div_
              [ HH.img [ HP.src expoData.urls.phrases_ms ]
              , renderTranslation state
              ]
            ]
            <> renderActive state
            <> renderMarkers state
          )
        ]
        ( [ renderHead state
          , renderFeedback state
          ]
          <> Nav.renderEndButtonsOr GoTo myself (Just TryCEAP) Resume state
        )

    eval :: Query ~> H.ComponentDSL State Query Void (M eff)
    eval (GoTo route next) = do
        options <- H.gets _.options
        Nav.evalGoToExtra options route next
    eval (Resume next) = Nav.evalResume next
    eval (Clicked ident next) = do
        -- One of the divisions has been clicked:
        --     If the ident is not already in found, add it to found
        --     Always make thisOne = ident (causes highlight in red)
        soFar <- H.gets _.found
        if elem ident soFar
          then H.modify (_ { thisOne = Just ident } )
          else H.modify (\s -> s { thisOne = Just ident
                                 , found = s.found `snoc` ident
                                 }
                        )
        pure next
    eval (ShowMe next) = do
        -- The "Show me" button simulates a click of the first remaining one
        soFar <- H.gets _.found
        let more = (0 .. (length gameData - 1)) `difference` soFar
        case index more 0 of
            Just n  -> eval (Clicked n next)
            Nothing -> pure next
