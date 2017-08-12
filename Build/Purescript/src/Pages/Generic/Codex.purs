-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.Codex (
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
      base_image:   expoData.locUrls.codex_ms
    , video:        expoData.locVideos.codicology
    , transcript:   expoData.locTalks.codicology
    , timings:      expoData.locTimings.codicology
    , sequence:     \_ -> talkSequence
    , myself:       Codex
    , continue:     Just UseOfMS
    }

talkSequence :: TalkSequenceType
talkSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "codex-item1",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.codex_item1])
    }
  , { name:   "codex-item2",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.codex_item2])
    }
  , { name:   "codex-pre-gap",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.codex_pre_gap])
    }
  , { name:   "codex-post-gap",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.codex_post_gap])
    }
  , { name:   "codex-stitching",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.codex_stitching])
    }
  ]
