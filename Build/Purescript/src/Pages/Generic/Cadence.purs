-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.Cadence (
    spec
) where

import Data.Maybe                  (Maybe(..))

import Base.Types                  (TalkSequence(..))
import Base.Router                 (Routes(..))
import Pages.Generic.Types         (Query(..), TalkSpec, TalkSequenceType)
import Pages.Shared.TalkPage       (StdAction(..))
import Base.ExpoData               (expoData)

spec :: TalkSpec
spec = {
      base_image:   expoData.locUrls.cadence_ms
    , video:        expoData.locVideos.cadence
    , transcript:   expoData.locTalks.cadence
    , timings:      expoData.locTimings.cadence
    , sequence:     \_ -> talkSequence
    , myself:       Cadence
    , continue:     Just Phrases
    }

talkSequence :: TalkSequenceType
talkSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.cadence_ms)
    }
  , { name:   "cadence-final",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.cadence_final])
    }
  , { name:   "cadence-other",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.cadence_other])
    }
  , { name:   "cadence-find",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.cadence_ms_game)
    }
  , { name:   "cadence-find-eg",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.cadence_eg])
    }
  , { name:   "cadence-find-game",
      reset:  false,
      action: Game { right:  expoData.locActives.cadence_right
                   , wrong:  expoData.locActives.cadence_wrong
                   , answer: expoData.locHLs.cadence_ans
                   }
    }
  ]
