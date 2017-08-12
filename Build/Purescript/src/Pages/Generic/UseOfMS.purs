-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.UseOfMS (
    spec
) where

import Prelude

import Data.Maybe                  (Maybe(..))

import Base.Types                  (Location(..), Language(Es),
                                    TalkSequence(..))
import Base.Router                 (Routes(..))
import Pages.Generic.Types         (Query(..), TalkSpec, TalkSequenceType)
import Pages.Shared.TalkPage       (StdAction(..))
import Base.ExpoData               (expoData)

spec :: TalkSpec
spec = {
      base_image:   expoData.locUrls.useofms_ms1
    , video:        expoData.locVideos.useofms
    , transcript:   expoData.locTalks.useofms
    , timings:      expoData.locTimings.useofms
    , sequence:     talkSequence
    , myself:       UseOfMS
    , continue:     Just Survival
    }

talkSequence :: Location -> TalkSequenceType
talkSequence location = TalkSequence
    if location == Madrid
      then mainSequence <> extraSequence
      else mainSequence
  where
    mainSequence =
      [ { name:   "talk-beginning",
          reset:  true,
          action: Standard (ResetWithBase expoData.locUrls.useofms_ms1)
        }
      , { name:   "useofms-use1",
          reset:  true,
          action: Standard (ResetWithBase expoData.locUrls.useofms_ms1)
        }
      -- Unfortunately the Spanish version of the Leon talk does not give an
      -- opportunity to show the chants and prayers.  (Portuguese does, but
      -- they are in the wrong order, so timing has to be simultaneous.)
      , { name:   "useofms-chant",
          reset:  false,
          action: CondAction (\loc lang ->
                      if loc == Leon && lang == Es
                        then DoHighlight []
                        else DoHighlight [expoData.locHLs.useofms_chant]
                  )
        }
      , { name:   "useofms-prayer",
          reset:  false,
          action: CondAction (\loc lang ->
                      if loc == Leon && lang == Es
                        then DoHighlight []
                        else DoHighlight [expoData.locHLs.useofms_prayer]
                  )
        }
      , { name:   "useofms-reading",
          reset:  false,
          action: Standard (DoHighlight [expoData.locHLs.useofms_reading])
        }
      , { name:   "useofms-add1",
          reset:  false,
          action: Standard (DoHighlight [expoData.locHLs.useofms_add1old,
                                         expoData.locHLs.useofms_add1new]
                           )
        }
      , { name:   "useofms-add2",
          reset:  false,
          action: Standard (DoHighlight [expoData.locHLs.useofms_add2old,
                                         expoData.locHLs.useofms_add2new]
                           )
        }
      , { name:   "useofms-marks",
          reset:  false,
          action: Standard (DoHighlight [expoData.locHLs.useofms_marks1,
                                         expoData.locHLs.useofms_marks2])
        }
      ]
    extraSequence =
      [ { name:   "useofms-use2",
          reset:  true,
          action: Standard (ResetWithBase expoData.locUrls.useofms_ms2)
        }
      , { name:   "useofms-add-ext",
          reset:  false,
          action: Standard (DoHighlight [expoData.locHLs.useofms_add_ext])
        }
      ]
