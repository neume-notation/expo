-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Generic.Music (
    spec
) where

import Prelude

import Data.Maybe                  (Maybe(..))
import Data.Nullable               (toMaybe, toNullable)

import Base.Types                  (Language(En), TalkSequence(..))
import Base.Router                 (Routes(..))
import Pages.Generic.Types         (Query(..), TalkSpec, TalkSequenceType)
import Pages.Shared.TalkPage       (StdAction(..))
import Base.ExpoData               (expoData, LocVideo, LocLangUrl(..),
                                    LocTalk(..), LocVidTime(..))

-- The Generic component expects LocVideo, LocTalk and LocVidTime values!
--
locVideo :: LocVideo
locVideo =
    let lwebm = expoData.videos.music.webm
        lmp4  = expoData.videos.music.mp4
    in { webm: LocLangUrl { coimbra: lwebm, lamego: lwebm,
                            leon: lwebm, madrid: lwebm }
       , mp4:  case toMaybe lmp4 of
                 Nothing -> toNullable Nothing
                 Just m  -> toNullable $ Just $
                              LocLangUrl { coimbra: m, lamego: m,
                                           leon: m, madrid: m }
       }

locTalk :: LocTalk
locTalk =
    let talk = expoData.talks.music
    in LocTalk { coimbra: talk, lamego: talk, leon: talk, madrid: talk }

locTimes :: LocVidTime
locTimes =
    let times = expoData.timings.music
    in LocVidTime { coimbra: times, lamego: times, leon: times, madrid: times }

spec :: TalkSpec
spec = {
      base_image:   expoData.locUrls.music_ms
    , video:        locVideo
    , transcript:   locTalk
    , timings:      locTimes
    , sequence:     \_ -> talkSequence
    , myself:       Music
    , continue:     Just Cadence
    }

talkSequence :: TalkSequenceType
talkSequence = TalkSequence
  [ { name:   "talk-beginning",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "music-up-down",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "music-torculus",
      reset:  false,
      -- Unfortunately the torculus didn't survive going into Spanish (:-), so
      -- the Spanish, and Portuguese which is derived from it, use a pes.
      action: CondAction (\_ lang -> case lang of
                               En -> DoHighlight [expoData.locHLs.music_NHL]
                               _  -> DoHighlight [expoData.locHLs.music_NH_w]
                         )
    }
  , { name:   "music-gaps",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "music-compound",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.music_compound,
                                     expoData.locHLs.music_compound2
                                    ])
    }
  , { name:   "music-shape",
      reset:  true,
      action: Standard Reset
    }
  , { name:   "music-variety",
      reset:  false,
      action: Standard (DoHighlight [expoData.locHLs.music_NH_a,
                                     expoData.locHLs.music_NH_w,
                                     expoData.locHLs.music_shape3,
                                     expoData.locHLs.music_shape4
                                    ])
    }
  , { name:   "music-mnemonic",
      reset:  true,
      action: Standard Reset
    }
  ]
