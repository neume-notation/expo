-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.TextBits (
    spec
) where

import Data.Maybe                  (Maybe(..))

import Base.Types                  (Location(..), Language(Pt),
                                    TalkSequence(..))
import Base.Router                 (Routes(..))
import Pages.Generic.Types         (Query(..), TalkSpec, TalkSequenceType)
import Pages.Shared.TalkPage       (StdAction(..))
import Base.ExpoData               (expoData)

spec :: TalkSpec
spec = {
      base_image:   expoData.locUrls.textbits_ms1
    , video:        expoData.locVideos.textbits
    , transcript:   expoData.locTalks.textbits
    , timings:      expoData.locTimings.textbits
    , sequence:     talkSequence
    , myself:       TextBits
    , continue:     Nothing
    }

-- The text-elements talks, both this one and TextMore, have very little
-- in common between the manuscripts, and any attempt to find common
-- subsequences seems to add complexity rather than reduce it!  Therefore
-- the sequences are separate, and the names for sections, highlights and
-- base images are kept generic and are re-used for unrelated purposes.
-- For example, `textbits-hl1` might be the highlight of "PR" (for Pater
-- noster) in one manuscript and a Gospel reading in another.
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
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-main",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl1])
    }
  , { name:   "textbits-verse",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl2])
    }
  , { name:   "textbits-repeat",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl3,
                                     expoData.locHLs.textbits_hl4])
    }
  , { name:   "textbits-rep-gloria",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl5])
    }
  , { name:   "textbits-hymn",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl6])
    }
  , { name:   "textbits-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-completuria",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl7])
    }
  , { name:   "textbits-newbase",
      reset:  false,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
      -- This is *not* a reset step, even though the action name looks as if
      -- it should be: we use this action since there is a previous highlight
      -- which needs to be cleared when resetting the base.
    }
  , { name:   "textbits-hl-pr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl8])
    }
  , { name:   "textbits-benedictio",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl9])
    }
  , { name:   "textbits-ad-m",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl10])
    }
  , { name:   "textbits-aeterne",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl11])
    }
  , { name:   "textbits-psalm-3",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl12])
    }
  , { name:   "textbits-psalm-50",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl13])
    }
  , { name:   "textbits-psalm-56",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl14])
    }
  , { name:   "textbits-erit",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl15])
    }
  , { name:   "textbits-erit-verse",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl16])
    }
  , { name:   "textbits-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-cont-hl",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl15,
                                     expoData.locHLs.textbits_hl16])
    }
  , { name:   "textbits-clear",
      reset:  false,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
      -- This is *not* a reset step, even though the action name looks as if
      -- it should be, and it does not even change the base image!  This
      -- action is here purely to clear the continued highlight, which
      -- actually belongs to the previous section - when the video is
      -- playing, the highlight needs to be held into this section, but
      -- when scrolling text, it should be cleared.
    }
  , { name:   "textbits-3",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  ]

lamegoSequence :: TalkSequenceType
lamegoSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-gospel",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl1])
    }
  , { name:   "textbits-praise",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl2])
    }
  , { name:   "textbits-scr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl3])
    }
  , { name:   "textbits-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms3)
    }
  , { name:   "textbits-newbase",
      reset:  false,
      action: Standard (SetBase expoData.locUrls.textbits_ms4)
    }
  , { name:   "textbits-sono",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl4,
                                     expoData.locHLs.textbits_hl5])
    }
  , { name:   "textbits-un-sono",
      reset:  false,
      action: Standard (UnHighlight expoData.locHLs.textbits_hl5)
      -- Leave the arrow, but remove the bar showing the extent
    }
  , { name:   "textbits-laudate",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl6,
                                     expoData.locHLs.textbits_hl7])
    }
  , { name:   "textbits-un-laudate",
      reset:  false,
      action: Standard (UnHighlight expoData.locHLs.textbits_hl7)
      -- Leave the arrow, but remove the bar showing the extent
    }
  , { name:   "textbits-lectio",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl8])
    }
  ]

leonSequence :: TalkSequenceType
leonSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-hl-pr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl1])
    }
  , { name:   "textbits-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-hl-pr-cont",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl1])
      -- Re-show the previous section's highlight.
    }
  , { name:   "textbits-newbase",
      reset:  false,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
      -- This is *not* a reset step, even though the action name looks as if
      -- it should be: we use this action since there is a previous highlight
      -- which needs to be cleared when resetting the base.
    }
  , { name:   "textbits-amen",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl2])
    }
  , { name:   "textbits-respond",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl3])
    }
  -- Unfortunately the Portuguese version was based on the old script, which
  -- did not identify the parts of the responsory.
  , { name:   "textbits-verse",
      reset:  false,
      action: CondAction (\_ lang -> case lang of
                               Pt -> DoHighlight []
                               _  -> DoHighlight [expoData.locHLs.textbits_hl4]
                         )
    }
  , { name:   "textbits-repeat",
      reset:  false,
      action: CondAction (\_ lang -> case lang of
                               Pt -> DoHighlight []
                               _  -> DoHighlight [expoData.locHLs.textbits_hl5,
                                                  expoData.locHLs.textbits_hl6]
                         )
    }
  , { name:   "textbits-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-prayer",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl7])
    }
  ]

madridSequence :: TalkSequenceType
madridSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms1)
    }
  , { name:   "textbits-eucharist",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl1,
                                     expoData.locHLs.textbits_hl2])
    }
  , { name:   "textbits-post-nom",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl3])
    }
  , { name:   "textbits-ad-pacem",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl4])
    }
  , { name:   "textbits-more-eu",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl5])
    }
  , { name:   "textbits-1",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-completuria",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl6])
    }
  , { name:   "textbits-pr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl7])
    }
  , { name:   "textbits-bene",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl8])
    }
  , { name:   "textbits-vespers",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl9])
    }
  , { name:   "textbits-2",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-ant",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl10])
    }
  , { name:   "textbits-ant-verse",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl11])
    }
  , { name:   "textbits-alleluia",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl12])
    }
  , { name:   "textbits-3",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-adm",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl13])
    }
  , { name:   "textbits-chant1",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl14])
    }
  , { name:   "textbits-chant2",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl15])
    }
  , { name:   "textbits-chant3",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl16])
    }
  , { name:   "textbits-chant4",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl17])
    }
  , { name:   "textbits-2nd-vr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl18])
    }
  , { name:   "textbits-2nd-adm",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl19])
    }
  , { name:   "textbits-4",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms2)
    }
  , { name:   "textbits-newbase",
      reset:  false,
      action: Standard (SetBase expoData.locUrls.textbits_ms3)
    }
  , { name:   "textbits-canticle",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl20])
    }
  , { name:   "textbits-xmas-bene",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl21])
    }
  , { name:   "textbits-xmas-sono",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl22])
    }
  , { name:   "textbits-xmas-laudes",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl23])
    }
  , { name:   "textbits-xmas-hymn",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl24])
    }
  , { name:   "textbits-xacrostic",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl25,
                                     expoData.locHLs.textbits_hl26])
    }
  , { name:   "textbits-5",
      reset:  true,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms3)
    }
  , { name:   "textbits-cont-hl",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl25,
                                     expoData.locHLs.textbits_hl26])
    }
  , { name:   "textbits-xmas-completuria",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl27])
    }
  , { name:   "textbits-newbase2",
      reset:  false,
      action: Standard (ResetWithBase expoData.locUrls.textbits_ms4)
      -- This is *not* a reset step, even though the action name looks as if
      -- it should be: we use this action since there is a previous highlight
      -- which needs to be cleared when resetting the base.
    }
  , { name:   "textbits-xmas-pr",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl28])
    }
  , { name:   "textbits-xmas-bene2",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl29])
    }
  , { name:   "textbits-decorated",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.textbits_hl30])
    }
  ]
