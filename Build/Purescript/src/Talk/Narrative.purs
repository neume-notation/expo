-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Talk.Narrative (
    Msg(..),
    VideoList,
    Query(..),
    ui
) where

-- This has many of the characteristics of a "third party" component because
-- it interfaces to the jQuery implementation in JSModules/narrative.js.
-- However, unlike the pure third-party example in the Halogen repository,
-- it renders a significant amount of HTML itself as well: it implements
-- the video-control buttons, whose actions are closely tied to the
-- Narrative object, and, for consistency of layout, provides the other
-- buttons in the same column, but these are simply provided as a service
-- for the parent component.

import Prelude

import Control.Monad.Eff           (Eff)
import Data.Array                  ((:))
import Data.Maybe                  (Maybe(..))
import DOM                         (DOM())
import DOM.HTML.Types              (HTMLElement)

import CSS.Geometry as Geometry
import CSS.Size as Size
import Halogen as H
import Halogen.Aff                 (HalogenEffects)
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Language, Timing, M)
import Base.ExpoData               (expoData, pick)
import Base.CssClasses as Class
import Talk.Dispatcher             (Dispatcher)

type VideoList = Array { url :: String, mimeType :: String }
type NarrativeSpec r = { video     :: VideoList
                       , text      :: String
                       , timing    :: Timing
                       , startAt   :: String
                       , vidHidden :: Boolean
                       | r
                       }

-- An opaque type for references to the Narrative object
foreign import data Narrative :: Type

foreign import createNarrative ::
    forall eff a r. HTMLElement -> NarrativeSpec r -> Dispatcher a
                    -> Eff (dom :: DOM | eff) Narrative

foreign import destroyNarrative ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit

-- `onWidget state.narrative` registers handler for events generated in the
-- foreign Narrative widget: these are size changes and video state-change
-- events (just those initiated in the Narrative object - see the comments
-- in JSModules/narrative.js).  It can be used as the first argument to
-- `eventSource` - note that it is in Eff, so must return a function which
-- performs the actual registration.  The second argument to `eventSource`
-- is the actual handler, ie it is the thing here which has type
-- `{state :: String, time :: Number} -> Eff (HalogenEffects eff) Unit`
foreign import onWidget ::
    forall eff. Narrative
                -> ( ( {state :: String, time :: Number}
                       -> Eff (HalogenEffects eff) Unit
                     )
                     -> Eff (HalogenEffects eff) Unit
                   )

foreign import playVideo ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit
foreign import pauseVideo ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit
foreign import hideVideo ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit
foreign import unhideVideo ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit
foreign import resize ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Unit
foreign import isVideoHidden ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Boolean
foreign import isVideoPlaying ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Boolean
foreign import videoTime ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Number
foreign import textMidHeight ::
    forall eff. Narrative -> Eff (dom :: DOM | eff) Number

-- The number in the Paused state is the time in seconds at which the video
-- is paused.  The state NearEnd means the same as Playing for purposes other
-- than the end message.
--
data PauseState = Playing | Paused Number | NearEnd | Ended

-- Note that this State is compatible with the `NarrativeSpec r` which needs
-- to be passed to the foreign constructor
type State a = { narrative  :: Maybe Narrative
               , language   :: Language
               , video      :: VideoList
               , text       :: String
               , timing     :: Timing
               , startAt    :: String
               , vidHidden  :: Boolean
               , pauseState :: PauseState
               , talkMiddle :: Number
               , buttons    :: Boolean    -- Just show/hide at present
               , buttonCls  :: Array HH.ClassName
               , buttonMsg  :: String
               , buttonAct  :: String     -- FIXME work out a better type!
               , dispatcher :: Maybe (Dispatcher a)
                               -- Can only be set on initialisation
             }

type Input a = NarrativeSpec ( language :: Language
                             , dispatcher :: Maybe (Dispatcher a)
                             )

data Msg = TellButton String

initialState :: forall a. Input a -> State a
initialState init = {
                 narrative:  Nothing
               , language:   init.language
               , video:      init.video
               , text:       init.text
               , timing:     init.timing
               , startAt:    init.startAt
               , vidHidden:  init.vidHidden
               , pauseState: Playing   -- Overridden in Initialize
               , talkMiddle: 600.0     -- A safe value pending proper setting
               , buttons:    false
               , buttonCls:  [ Class.green_button ]
               , buttonMsg:  ""
               , buttonAct:  ""
               , dispatcher: init.dispatcher
               }

data Query a =   Initialize a
               | Finalize a
               | ToggleHide a        -- Clicked the hide/show video button
               | IsHidden (Boolean -> a)
               | PauseGo a           -- Clicked the pause/resume/play button
               | Foreign String Number a  -- Changes arising in the Narrative object
               | DetailButtons (Array HH.ClassName) String String a
               | NoDetailButtons a
               | EarlyEnd a
               | ButtonClicked String a

-- In certain situations display a message on top of the video: if the video
-- is ended, display an encouragement to navigate further, and if it is
-- paused right at the start, display a placeholder to ensure there is not
-- a complete blank, for example when srolling back to the first section.
--
videoMessage :: forall a. State a -> Array (H.ComponentHTML Query)
videoMessage state =
    if state.vidHidden then
      []
    else
      case state.pauseState of
        Paused t -> if t < 0.1 then showMsg startMsg
                    else []
        Ended    -> showMsg endMsg
        NearEnd  -> showMsg endMsg
        Playing  -> []
  where
    showMsg msg =
      [ HH.div
        [ HP.class_ $ HH.ClassName "expo-video-msg" ]
        [ HH.div [ HP.class_ Class.vcentre ] [ HH.text msg ] ]
      ]
    startMsg = pick state.language expoData.messages.start_msg
    endMsg   = pick state.language expoData.messages.end_msg

ui :: forall eff a. H.Component HH.HTML Query (Input a) Msg (M eff)
ui = H.lifecycleComponent { initialState: initialState
                          , render
                          , eval
                          , receiver: const Nothing
                          , initializer: Just (H.action Initialize)
                          , finalizer: Just (H.action Finalize)
                          }
  where
    render :: State a -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.classes [ HB.row, Class.fullheight ] ]
        [ HH.div
          [ HP.class_ HB.colMd9 ]
          ( HH.div
            -- This is where we attach the jQuery narrative widget
            [ HP.ref $ H.RefLabel "narrative"
            , HP.class_ $ HH.ClassName "expo-video"
            ]
            []
          : videoMessage state
          )
        , HH.div
          [ HP.class_ HB.colMd3 ]
          [ HH.div
            [ HP.classes [ Class.controls_col, Class.tight_centre ] ]
            [ HH.div
              [ HP.class_ $ HH.ClassName "video-controls" ]
              [ HH.div_
                [ HH.button
                  [ HE.onClick (HE.input_ ToggleHide)
                  , HP.class_ Class.red_button
                  ]
                  [ HH.text
                      if state.vidHidden
                      then pick state.language expoData.messages.show_video
                      else pick state.language expoData.messages.hide_video
                  ]
                ]
              , HH.div
                [ HP.class_ Class.vseparate ]
                if state.vidHidden then
                  []  -- Don't show Pause button if the video is hidden
                else
                  [ HH.button
                    [ HE.onClick (HE.input_ PauseGo)
                    , HP.class_ case state.pauseState of
                                    Playing -> Class.orange_button
                                    NearEnd -> Class.orange_button
                                    _       -> Class.green_button
                    ]
                    [ HH.text
                        case state.pauseState of
                          Playing  -> pick state.language expoData.messages.pause_video
                          NearEnd  -> pick state.language expoData.messages.pause_video
                          Paused _ -> pick state.language expoData.messages.resume_video
                          Ended    -> pick state.language expoData.messages.replay_video
                    ]
                  ]
              ]
            , HH.div
              [ HP.class_ Class.detail_controls
              , CSS.style $ Geometry.top (Size.px (state.talkMiddle - 50.0))
              ]
              if state.buttons then
                [ HH.button
                  [ HE.onClick (HE.input_ (ButtonClicked state.buttonAct))
                  , HP.classes state.buttonCls
                  ]
                  [ HH.text state.buttonMsg ]
                ]
              else []
            ]
          ]
        ]


    eval :: Query ~> H.ComponentDSL (State a) Query Msg (M eff)
    eval (Initialize next) = do
        s <- H.get
        el <- H.getHTMLElementRef (H.RefLabel "narrative")
        -- Neither of these Nothing cases should happen: the dispatcher
        -- should be set by the parent in the initial state, and the
        -- RefLabel should be defined by DOM creation before we get here.
        case el of
          Just el' -> do
            case s.dispatcher of
              Just d -> do
                -- State is OK as NarrativeSpec
                n <- H.liftEff $ createNarrative el' s d
                tm <- H.liftEff $ textMidHeight n
                H.modify (_ { narrative = Just n, talkMiddle = tm } )
                H.subscribe $  H.eventSource (onWidget n) \status -> do
                    Just $ Foreign status.state status.time H.Listening
                H.liftEff $ playVideo n
                h <- H.liftEff $ isVideoHidden n
                p <- H.liftEff $ isVideoPlaying n
                t <- H.liftEff $ videoTime n
                let ps = if p then Playing else Paused t
                H.modify (_ { vidHidden = h, pauseState = ps } )
              Nothing -> pure unit
          Nothing -> pure unit
        pure next
    eval (Finalize next) = do
        n <- H.gets _.narrative
        H.modify (_ { narrative = Nothing } )         -- Remove object reference
        case n of
          Just n' -> H.liftEff $ destroyNarrative n'  -- Remove from DOM
          Nothing -> pure unit
        pure next
    eval (ToggleHide next) = do
        n <- H.gets _.narrative
        case n of
          Just n' -> do
            hidden <- H.liftEff $ isVideoHidden n'
            if hidden
              then do
                  H.liftEff $ unhideVideo n'
                  H.modify (_ { pauseState = Playing } )
              else do
                  H.liftEff $ hideVideo n'
                  t <- H.liftEff $ videoTime n'
                  H.modify (_ { pauseState = Paused t } )
            H.liftEff $ resize n'
            tm <- H.liftEff $ textMidHeight n'
            H.modify (_ { vidHidden = not hidden, talkMiddle = tm } )
          Nothing -> pure unit
        pure next
    eval (IsHidden continue) = do
        n <- H.gets _.narrative
        result <- case n of
                    Just n' -> H.liftEff $ isVideoHidden n'
                    _       -> pure false  -- Shouldn't happen!
        pure (continue result)
    eval (PauseGo next) = do
        n <- H.gets _.narrative
        case n of
          Just n' -> do
            p <- H.liftEff $ isVideoPlaying n'
            if p
              then do
                H.liftEff $ pauseVideo n'
                t <- H.liftEff $ videoTime n'
                H.modify (_ { pauseState = Paused t } )
              else do
                H.liftEff $ playVideo n'
                H.modify (_ { pauseState = Playing } )
          Nothing -> pure unit
        pure next
    eval (Foreign status t next) = do
        s <- H.get
        let pauseState = if status == "ended" then Ended
                         else if status == "paused" then Paused t
                         else s.pauseState
        tm <- case s.narrative of
                Just n -> H.liftEff $ textMidHeight n
                Nothing -> pure s.talkMiddle
        H.modify (_ { pauseState = pauseState, talkMiddle = tm } )
        pure next
    eval (DetailButtons cls msg act next) = do
        -- Query from parent
        H.modify (_ { buttons = true, buttonCls = cls,
                      buttonMsg = msg, buttonAct = act } )
        pure next
    eval (NoDetailButtons next) = do
        -- Query from parent
        H.modify (_ { buttons = false } )
        pure next
    eval (EarlyEnd next) = do
        -- Query from parent.  This is the only way pauseState = NearEnd can
        -- be set, and it is done only if the video is actually playing (not
        -- paused).  For the purpose of showing the end message only, NearEnd
        -- is treated like Ended, otherwise it is like Playing.  This is an
        -- unfortunate special case for the English introduction, where the
        -- final part of the video is blank, and the end message needs to be
        -- shown early.
        n <- H.gets _.narrative
        case n of
          Just n' -> do
            p <- H.liftEff $ isVideoPlaying n'
            if p then H.modify (_ { pauseState = NearEnd } )
                 else pure unit
          Nothing -> pure unit
        pure next
    eval (ButtonClicked which next) = do
        -- Just pass message to parent
        H.raise $ TellButton which
        pure next
