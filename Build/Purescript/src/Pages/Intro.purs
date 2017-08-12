-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Intro (
    Query,
    ui
) where

import Prelude

import Data.Array                  ((:))
import Data.Map                    (empty, singleton, delete)
import Data.Maybe                  (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Base.Types                  (Location, Language(..),
                                    Timing, TalkSequence(..), M)
import Base.Router                 (Routes(..), Extra, setRoute)
import Base.ExpoData               (expoData, pick, pickLoc, LocUrl)
import Base.CssClasses as Class
import Pages.Shared.TalkPage       (StdAction(..))
import Pages.Shared.TalkPage as Shared
import Pages.Shared.EndButtons as Nav
import Pages.Shared.Layout as Layout
import Pages.Shared.Types as T
import Talk.Narrative as N

myself :: Location -> Language -> Extra -> Routes
myself = Intro

data Query a =   Standard (StdAction Query) a
               | HandleMessage N.Msg a
               | PartialReset LocUrl a
               | PhraseTable a
               | ShowDetail String a
               | ShowEnd a
               | GoTo Nav.RouteSpec a

-- Types building in this components action type
--
type TalkSequenceType = TalkSequence Query

type State = T.TalkState Query (
               resumeAt   :: String   -- See initialisation and renderNarrative
             , phrases    :: Boolean
             , endButtons :: Boolean
             )

initialState :: T.PageState () -> State
initialState init = {
                 location:   init.location
               , language:   init.language
               , options:    init.options
               , resumeAt:   fromMaybe "" $ init `T.option` T.optIntroPos
               , base_image: expoData.locUrls.iberia
               , phrases:    false
               , highlights: []
               , endButtons: false
               , dispatcher: Nothing
               }

-- Data defining this talk

videos :: Location -> Language -> N.VideoList
videos = Shared.videoList expoData.locVideos.introduction

transcript :: Location -> Language -> String
transcript loc lang = pick lang $ pickLoc loc expoData.locTalks.introduction

timings :: Location -> Language -> Timing
timings loc lang = pick lang $ pickLoc loc expoData.locTimings.introduction

talkSequence :: Location -> TalkSequenceType
talkSequence _ = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.iberia
    }
  , { name:   "intro-the-ms",
      reset:  false,
      action: Standard $ SetBase expoData.locUrls.intro_ms
    }
  , { name:   "intro-text-elements",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.intro_ms
    }
  , { name:   "intro-hl-text",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_hl_text]
    }
  , { name:   "intro-hl-music",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_hl_music]
    }
  , { name:   "intro-codex-pre",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.intro_ms
    }
  , { name:   "intro-codex",
      reset:  true,
      action: PartialReset expoData.locUrls.intro_ms
    }
  , { name:   "intro-codex-buttons",
      reset:  false,
      action: ShowDetail "codex"
    }
  , { name:   "intro-text-about-pre",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.intro_ms
    }
  , { name:   "intro-text-about",
      reset:  true,
      action: PartialReset expoData.locUrls.intro_ms
    }
  , { name:   "intro-text-writing",
      reset:  false,
      action:  Standard $ DoHighlight [expoData.locHLs.intro_writing]
    }
  , { name:   "intro-text-buttons",
      reset:  false,
      action: ShowDetail "text"
    }
  , { name:   "intro-notation-pre",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.music_ms
    }
  , { name:   "intro-notation",
      reset:  true,
      action: PartialReset expoData.locUrls.music_ms
    }
  , { name:   "intro-notation-shapes",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_neume1,
                                      expoData.locHLs.intro_neume2
                                     ]
    }
  , { name:   "intro-notation-buttons",
      reset:  false,
      action: ShowDetail "notation"
    }
  , { name:   "intro-pace-long",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_long_syl]
    }
  , { name:   "intro-pace-short",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_short_syl]
    }
  , { name:   "intro-cadences",
      reset:  true,
      action: Standard $ ResetWithBase expoData.locUrls.intro_phrases
    }
  , { name:   "intro-phrases",
      reset:  false,
      action: PhraseTable
    }
  , { name:   "intro-phrase-ends",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_phr_prxmo,
                                      expoData.locHLs.intro_phr_dmnus
                                     ]
    }
  , { name:   "intro-phrase-music",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_proximo,
                                      expoData.locHLs.intro_dominus
                                     ]
    }
  , { name:   "intro-phrase-short1",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_phr_sbrie]
    }
  , { name:   "intro-phrase-short2",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_phr_vobis]
    }
  , { name:   "intro-phrase-short3",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_phr_terra]
    }
  , { name:   "intro-phrase-long",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_phr_mala]
    }
  , { name:   "intro-phrase-creabo",
      reset:  false,
      action: Standard $ DoHighlight [expoData.locHLs.intro_creabo]
    }
  , { name:   "intro-end",
      reset:  false,
      action: ShowEnd
    }
  ]

-- The name of the first step in `talkSequence`
beginning :: String
beginning = "talk-beginning"

-- The name of the step in `talkSequence` at which we should start when
-- coming back to the intro from the codicology more-detail sections.
resume_codex :: String
resume_codex = "intro-codex-buttons"

-- Similarly the text sections
resume_text :: String
resume_text = "intro-text-buttons"

-- Similarly the notation sections
resume_notation :: String
resume_notation = "intro-notation-buttons"

-- There's only one child (necessarily of a single type (!)), uniquely tied
-- to this component, so the slot address needs only one value.
type NarrativeSlot = Unit

-- Display a table showing how the text of a specific chant (in BN01) is
-- divided into phrases.  It is an SVG image, but it depends on the language.
--
phraseTable :: forall p i. State -> Array (HH.HTML p i)
phraseTable state =
    if state.phrases then theTable
    else []
  where
    theTable =
      [ HH.img
        [ HP.src case state.language of
                   En -> expoData.urls.phrases_en
                   Es -> expoData.urls.phrases_es
                   Pt -> expoData.urls.phrases_pt
        , HP.class_ Class.at_bottom
        , HP.width 810
        ]
      ]

ui :: forall eff. H.Component HH.HTML Query (T.PageState ()) Void (M eff)
ui = H.lifecycleParentComponent {
       initialState: initialState
     , render
     , eval
     , receiver: const Nothing
     , initializer: Just $ H.action (Standard $ Initialize talkSequence Nothing)
     , finalizer: Just $ H.action (Standard Finalize)
     }
  where
    render :: State -> H.ParentHTML Query N.Query NarrativeSlot (M eff)
    render state =
      let loc = state.location
      in
      Layout.renderTwoCol
          [ HH.div
            [ HP.classes [ Class.vcentre, Class.tight_centre ] ]
            ( HH.img
              [ HP.src $ pickLoc state.location state.base_image]
              : Shared.renderHighlights state
              <> phraseTable state
            )
          ]
          ( Shared.renderNarrative unit state (videos loc) (transcript loc) (timings loc) state.resumeAt HandleMessage
            <> Nav.renderEndButtons GoTo myself Nothing state
          )

    eval :: Query ~> H.ParentDSL State Query N.Query NarrativeSlot Void (M eff)
    eval (Standard action@(Initialize _ _) next) = do
      -- If `optIntroPos` was set in the initialisation input data, it has
      -- now been copied to state.resumeAt, so it can be deleted from the
      -- options to avoid it making `renderEndButtons` work incorrectly.
      H.modify (\s -> s { options = delete T.optIntroPos s.options })
      Shared.evalStandard action next
    eval (Standard Reset next) = myReset true Reset next
    eval (Standard (ResetWithBase b) next) = myReset true (ResetWithBase b) next
    eval (Standard action next) = Shared.evalStandard action next
    eval (HandleMessage (N.TellButton which) next) = do
      -- We must check the hidden state when the button is clicked, not
      -- when it is rendered!
      -- The return position is uniquely determined by which button was
      -- clicked, so no need to query the Narrative for the section!
      s <- H.get
      let dest =
            if which == "codex"
                then { page: Codex, resume: resume_codex }
            else if which == "notation"
                then { page: Music, resume: resume_notation }
            else if which == "text"
                then { page: ReadText, resume: resume_text }
            else { page: Explore, resume: beginning } -- Shouldn't happen!
      let pos = singleton T.optIntroPos dest.resume
      extra <- Shared.evalHidden pos
      H.liftEff $ setRoute $ dest.page s.location s.language extra
      pure next
    eval (PartialReset b next) = myReset false (ResetWithBase b) next
    eval (PhraseTable next) = do
      H.modify (_ { phrases = true } )
      pure next
    eval (ShowDetail d next) = do
      s <- H.get
      let label = if d == "codex" then expoData.messages.ms_usage
                  else if d == "notation" then expoData.messages.not_intro
                  else if d == "text" then expoData.messages.txt_bits
                  else expoData.messages.ms_usage -- Shouldn't happen!
      _ <- H.query unit $ H.action (N.DetailButtons [ Class.green_button ]
                                      (pick s.language label)
                                      d
                                   )
      pure next
    eval (ShowEnd next) = do
      -- Invoke the EarlyEnd action at the same time as showing the buttons
      -- (but it has no effect unless the video is actually playing).
      _ <- H.query unit $ H.action N.EarlyEnd
      H.modify (\s -> s { endButtons = true })
      pure next
    eval (GoTo route next) = do
      hiddenExtra <- Shared.evalHidden empty
      Nav.evalGoToExtra hiddenExtra route next

    -- Resets require extra actions in addition to the standard ones.
    -- Only the removal of details buttons is omitted from partial resets.
    myReset :: forall e n.
         Boolean
      -> StdAction Query
      -> n
      -> H.ParentDSL State Query N.Query NarrativeSlot Void (M e) n
    myReset fullReset action next = do
      H.modify (_ { phrases = false, endButtons = false })
      when fullReset $
          void $ H.query unit $ H.action N.NoDetailButtons
      Shared.evalStandard action next
