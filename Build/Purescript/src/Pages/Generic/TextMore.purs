-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.TextMore (
    spec
) where

import Data.Maybe                  (Maybe(..))

import Base.Types                  (Location(..), TalkSequence(..))
import Base.Router                 (Routes(..))
import Pages.Generic.Types         (Query(..), TalkSpec, TalkSequenceType)
import Pages.Shared.TalkPage       (StdAction(..))
import Base.ExpoData               (expoData)

spec :: TalkSpec
spec = {
      base_image:   expoData.locUrls.textmore_ms1
    , video:        expoData.locVideos.textmore
    , transcript:   expoData.locTalks.textmore
    , timings:      expoData.locTimings.textmore
    , sequence:     talkSequence
    , myself:       TextMore
    , continue:     Just TextBits
    }

-- The text-elements talks, both this one and TextBits, have very little
-- in common between the manuscripts, and any attempt to find common
-- subsequences seems to add complexity rather than reduce it!  Therefore
-- the sequences are separate, and the names for sections and highlights
-- are kept generic and are re-used for unrelated purposes.  For example,
-- `textmore-hl1` might be the highlight of "III" (roman numeral 3) in one
-- manuscript and a chant in another.
--
talkSequence :: Location -> TalkSequenceType
talkSequence loc =
    case loc of
      Coimbra -> coimbraSequence
      Lamego  -> lamegoSequence
      Leon    -> leonSequence
      Madrid  -> madridSequence

coimbraSequence :: TalkSequenceType
coimbraSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "textmore-1",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "textmore-begins",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl1,
                                     expoData.locHLs.textmore_hl2])
    }
  , { name:   "textmore-2",
      reset:  true,
      action: Standard Reset
    }
  ]

lamegoSequence :: TalkSequenceType
lamegoSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms1)
    }
  , { name:   "textmore-iii",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl1])
    }
  , { name:   "textmore-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms1)
    }
  , { name:   "textmore-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms2)
    }
  , { name:   "textmore-3",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms2)
    }
  , { name:   "textmore-exult",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl2,
                                     expoData.locHLs.textmore_hl3])
    }
  , { name:   "textmore-laus",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl4,
                                     expoData.locHLs.textmore_hl5])
    }
  , { name:   "textmore-margin",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl6])
    }
  ]

leonSequence :: TalkSequenceType
leonSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard Reset
    }
  ]

madridSequence :: TalkSequenceType
madridSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms1)
    }
  , { name:   "textmore-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms2)
    }
  , { name:   "textmore-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textmore_ms1)
    }
  , { name:   "textmore-chant",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl1])
    }
  , { name:   "textmore-prayer",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textmore_hl2])
    }
  ]
