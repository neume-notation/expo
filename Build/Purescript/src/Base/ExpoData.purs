-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.ExpoData where

import Prelude

import Base.Types                  (Location(..), Language(..), Timing)
import Data.Nullable               (Nullable)

newtype Message    = Message { en :: String, es :: String, pt :: String }

newtype LocUrl     = LocUrl {
                       coimbra         :: String        
                     , lamego          :: String
                     , leon            :: String
                     , madrid          :: String
                     }

newtype LangUrl    = LangUrl { en :: String, es :: String, pt :: String }

newtype LocLangUrl = LocLangUrl {
                       coimbra         :: LangUrl
                     , lamego          :: LangUrl
                     , leon            :: LangUrl
                     , madrid          :: LangUrl
                     }

type Video         = { webm :: LangUrl, mp4 :: Nullable LangUrl }

type LocVideo      = { webm :: LocLangUrl, mp4 :: Nullable LocLangUrl }

newtype Talk       = Talk { en :: String, es :: String, pt :: String }

newtype LocTalk    = LocTalk {
                       coimbra         :: Talk
                     , lamego          :: Talk
                     , leon            :: Talk
                     , madrid          :: Talk
                     }

newtype VidTime    = VidTime { en :: Timing, es :: Timing, pt :: Timing }

newtype LocVidTime = LocVidTime {
                       coimbra         :: VidTime
                     , lamego          :: VidTime
                     , leon            :: VidTime
                     , madrid          :: VidTime
                     }

type Rectangle = { x :: Number, y :: Number, w :: Number, h :: Number }

newtype Active = Active (Array Rectangle)

newtype LocActive = LocActive {
                         coimbra         :: Active
                       , lamego          :: Active
                       , leon            :: Active
                       , madrid          :: Active
                       }

newtype Highlight = Highlight { img :: String, geom :: Rectangle }

newtype LocHighlight = LocHighlight {
                         coimbra         :: Nullable Highlight
                       , lamego          :: Nullable Highlight
                       , leon            :: Nullable Highlight
                       , madrid          :: Nullable Highlight
                       }

-- Data specific to the TryCEAP activity
--
type Neume    = { neume :: String, x :: Int, y :: Int }
type ChantSyl = { ident :: Int, txt :: String, neumes :: Array Neume }
type CeapData = { html       :: String
                , transcript :: Array ChantSyl
                , similar    :: Array (Array String)
                }


-- Objects containing all known items
-- ----------------------------------
--
-- All items correspond to named properties.  Compared with using a dynamic
-- mapping type (eg Data.Map or Data.StrMap), this puts the complete list of
-- required entries into this one file, and all uses within Purescript are
-- guaranteed by the type checker.  The data provided by the server still
-- needs to be checked against this, but that is easier than finding
-- references distributed across the all of the client code.

type Messages      = { small_screen    :: Message
                     , small_full      :: Message
                     , small_zoom      :: Message
                     , greet_head      :: Message
                     , greet_head_alt  :: Message
                     , greeting        :: Message
                     , greeting_alt    :: Message
                     , alt_coimbra     :: Message
                     , alt_lamego      :: Message
                     , alt_leon        :: Message
                     , alt_madrid      :: Message
                     , start           :: Message
                     , menu            :: Message
                     , credits         :: Message
                     , credits_head    :: Message
                     , hide_video      :: Message
                     , show_video      :: Message
                     , pause_video     :: Message
                     , resume_video    :: Message
                     , replay_video    :: Message
                     , start_again     :: Message
                     , continue        :: Message
                     , back_button     :: Message
                     , start_msg       :: Message
                     , end_msg         :: Message
                     , ms_usage        :: Message
                     , not_intro       :: Message
                     , txt_bits        :: Message
                     , nav_head        :: Message
                     , nav_greet       :: Message
                     , nav_intro       :: Message
                     , nav_ms          :: Message
                     , nav_ms_like     :: Message
                     , nav_ms_usage    :: Message
                     , nav_ms_survive  :: Message
                     , nav_not         :: Message
                     , nav_not_intro   :: Message
                     , nav_not_cadence :: Message
                     , nav_not_ceap    :: Message
                     , nav_not_phrases :: Message
                     , nav_txt         :: Message
                     , nav_txt_bits    :: Message
                     , nav_txt_more    :: Message
                     , nav_txt_read    :: Message
                     , nav_alt         :: Message
                     , nav_alt_coimbra :: Message
                     , nav_alt_lamego  :: Message
                     , nav_alt_leon    :: Message
                     , nav_alt_madrid  :: Message
                     , nav_alt_this    :: Message
                     , nav_feedback    :: Message
                     , give_up         :: Message
                     , show_me         :: Message
                     , game_right      :: Message
                     , game_wrong      :: Message
                     , game_score      :: Message
                     , game_more       :: Message
                     , game_penult     :: Message
                     , game_next       :: Message
                     , survive_head    :: Message
                     , survive_para1   :: Message
                     , survive_para2   :: Message
                     , survive_opt0    :: Message
                     , survive_opt1    :: Message
                     , survive_opt2    :: Message
                     , survive_opt3    :: Message
                     , survive_opt4    :: Message
                     , survive_opt5    :: Message
                     , survive_ans0    :: Message
                     , survive_ans1    :: Message
                     , survive_ans2    :: Message
                     , survive_ans3    :: Message
                     , survive_ans4    :: Message
                     , survive_ans5    :: Message
                     , readtext_head   :: Message
                     , readtext_para1  :: Message
                     , readtext_para2  :: Message
                     , readtext_final  :: Message
                     , readtext_john   :: Message
                     , phrases_head    :: Message
                     , phrases_para1   :: Message
                     , phrases_para2   :: Message
                     , phrases_more    :: Message
                     , phrases_done    :: Message
                     , phrases_trans1  :: Message
                     , phrases_trans2  :: Message
                     , phrases_trans3  :: Message
                     , phrases_trans4  :: Message
                     , phrases_trans5  :: Message
                     , phrases_trans6  :: Message
                     , phrases_trans7  :: Message
                     , phrases_trans8  :: Message
                     , phrases_expl1   :: Message
                     , phrases_expl2   :: Message
                     , phrases_expl3   :: Message
                     , phrases_expl4   :: Message
                     , phrases_expl7   :: Message
                     , phrases_expl8   :: Message
                     , tryceap_head    :: Message
                     , tryceap_para1   :: Message
                     , tryceap_para2   :: Message
                     , tryceap_init    :: Message
                     , tryceap_again   :: Message
                     , tryceap_blanks  :: Message
                     , tryceap_count   :: Message
                     , tryceap_shape   :: Message
                     , tryceap_pos     :: Message
                     , tryceap_good    :: Message
                     }

type Urls          = { flag_spain      :: String
                     , flag_portugal   :: String
                     , flag_uk         :: String
                     , small_flag_es   :: String
                     , small_flag_pt   :: String
                     , small_flag_en   :: String
                     , tracker         :: String
                     , emoji_happy     :: String
                     , emoji_sad       :: String
                     , phrases_en      :: String
                     , phrases_es      :: String
                     , phrases_pt      :: String
                     , logo_ahrc       :: String
                     , logo_bne        :: String
                     , logo_bristol    :: String
                     , logo_coimbra    :: String
                     , logo_lamego     :: String
                     , logo_leon       :: String
                     , logo_valencia   :: String
                     , radio_on        :: String
                     , radio_off       :: String
                     , survive_ms0     :: String
                     , survive_ms1     :: String
                     , survive_ms2     :: String
                     , survive_ms3     :: String
                     , survive_ms4     :: String
                     , survive_ms5     :: String
                     , readtext_ms     :: String
                     , letters         :: String
                     , phrases_ms      :: String
                     , ceap_in_hoc     :: String
                     }

type LocUrls       = { mag_glass       :: LocUrl
                     , choose_loc      :: LocUrl
                     , iberia          :: LocUrl
                     , intro_ms        :: LocUrl
                     , intro_phrases   :: LocUrl
                     , music_ms        :: LocUrl
                     , cadence_ms      :: LocUrl
                     , cadence_ms_game :: LocUrl
                     , codex_ms        :: LocUrl
                     , useofms_ms1     :: LocUrl
                     , useofms_ms2     :: LocUrl
                     , textbits_ms1    :: LocUrl
                     , textbits_ms2    :: LocUrl
                     , textbits_ms3    :: LocUrl
                     , textbits_ms4    :: LocUrl
                     , textmore_ms1    :: LocUrl
                     , textmore_ms2    :: LocUrl
                     }

type LocHLs        = { intro_hl_text   :: LocHighlight
                     , intro_hl_music  :: LocHighlight
                     , intro_writing   :: LocHighlight
                     , intro_neume1    :: LocHighlight
                     , intro_neume2    :: LocHighlight
                     , intro_short_syl :: LocHighlight
                     , intro_long_syl  :: LocHighlight
                     , intro_phr_prxmo :: LocHighlight
                     , intro_phr_dmnus :: LocHighlight
                     , intro_phr_sbrie :: LocHighlight
                     , intro_phr_vobis :: LocHighlight
                     , intro_phr_terra :: LocHighlight
                     , intro_phr_mala  :: LocHighlight
                     , intro_proximo   :: LocHighlight
                     , intro_dominus   :: LocHighlight
                     , intro_creabo    :: LocHighlight
                     , music_NHL       :: LocHighlight
                     , music_compound  :: LocHighlight
                     , music_compound2 :: LocHighlight
                     , music_NH_a      :: LocHighlight
                     , music_NH_w      :: LocHighlight
                     , music_shape3    :: LocHighlight
                     , music_shape4    :: LocHighlight
                     , cadence_final   :: LocHighlight
                     , cadence_other   :: LocHighlight
                     , cadence_eg      :: LocHighlight
                     , cadence_ans     :: LocHighlight
                     , codex_item1     :: LocHighlight
                     , codex_item2     :: LocHighlight
                     , codex_pre_gap   :: LocHighlight
                     , codex_post_gap  :: LocHighlight
                     , codex_stitching :: LocHighlight
                     , useofms_chant   :: LocHighlight
                     , useofms_prayer  :: LocHighlight
                     , useofms_reading :: LocHighlight
                     , useofms_add1old :: LocHighlight
                     , useofms_add1new :: LocHighlight
                     , useofms_add2old :: LocHighlight
                     , useofms_add2new :: LocHighlight
                     , useofms_marks1  :: LocHighlight
                     , useofms_marks2  :: LocHighlight
                     , useofms_add_ext :: LocHighlight
                     , textbits_hl1    :: LocHighlight
                     , textbits_hl2    :: LocHighlight
                     , textbits_hl3    :: LocHighlight
                     , textbits_hl4    :: LocHighlight
                     , textbits_hl5    :: LocHighlight
                     , textbits_hl6    :: LocHighlight
                     , textbits_hl7    :: LocHighlight
                     , textbits_hl8    :: LocHighlight
                     , textbits_hl9    :: LocHighlight
                     , textbits_hl10   :: LocHighlight
                     , textbits_hl11   :: LocHighlight
                     , textbits_hl12   :: LocHighlight
                     , textbits_hl13   :: LocHighlight
                     , textbits_hl14   :: LocHighlight
                     , textbits_hl15   :: LocHighlight
                     , textbits_hl16   :: LocHighlight
                     , textbits_hl17   :: LocHighlight
                     , textbits_hl18   :: LocHighlight
                     , textbits_hl19   :: LocHighlight
                     , textbits_hl20   :: LocHighlight
                     , textbits_hl21   :: LocHighlight
                     , textbits_hl22   :: LocHighlight
                     , textbits_hl23   :: LocHighlight
                     , textbits_hl24   :: LocHighlight
                     , textbits_hl25   :: LocHighlight
                     , textbits_hl26   :: LocHighlight
                     , textbits_hl27   :: LocHighlight
                     , textbits_hl28   :: LocHighlight
                     , textbits_hl29   :: LocHighlight
                     , textbits_hl30   :: LocHighlight
                     , textmore_hl1    :: LocHighlight
                     , textmore_hl2    :: LocHighlight
                     , textmore_hl3    :: LocHighlight
                     , textmore_hl4    :: LocHighlight
                     , textmore_hl5    :: LocHighlight
                     , textmore_hl6    :: LocHighlight
                     }

type LocActives    = { cadence_right   :: LocActive
                     , cadence_wrong   :: LocActive
                     }

type Videos        = { music           :: Video
                     }

type LocVideos     = { introduction    :: LocVideo
                     , codicology      :: LocVideo
                     , useofms         :: LocVideo
                     , cadence         :: LocVideo
                     , textbits        :: LocVideo
                     , textmore        :: LocVideo
                     }

type Talks         = { music           :: Talk
                     , credits         :: Talk   -- Poor naming in this case!
                     }

type LocTalks      = { introduction    :: LocTalk
                     , codicology      :: LocTalk
                     , useofms         :: LocTalk
                     , cadence         :: LocTalk
                     , textbits        :: LocTalk
                     , textmore        :: LocTalk
                     }

type Timings       = { music           :: VidTime
                     }

type LocTimings    = { introduction    :: LocVidTime
                     , codicology      :: LocVidTime
                     , useofms         :: LocVidTime
                     , cadence         :: LocVidTime
                     , textbits        :: LocVidTime
                     , textmore        :: LocVidTime
                     }

type All           = { messages   :: Messages
                     , urls       :: Urls
                     , locUrls    :: LocUrls
                     , locHLs     :: LocHLs
                     , locActives :: LocActives
                     , videos     :: Videos
                     , locVideos  :: LocVideos
                     , talks      :: Talks
                     , locTalks   :: LocTalks
                     , timings    :: Timings
                     , locTimings :: LocTimings
                     , miniceap   :: CeapData
                     , feedback   :: Nullable LangUrl
                     }

foreign import expoData :: All


-- Instances as required
-- ---------------------

derive instance eqHighlight :: Eq Highlight
derive instance eqLocHighlight :: Eq LocHighlight


-- Selection of sub-entry by location (manuscript) or language
-- -----------------------------------------------------------
--
-- Where an item depends on both location and language, for example
-- the URLs for the videos in LocVideos, both `pickLoc` and `pick`
-- need to be used to walk down the tree.  For example:
--
--     pick Es $ pickLoc Leon expoData.locVideos.codicology.webm

class PickLang a b | a -> b where
    pick :: Language -> a -> b

instance pickLangMessage :: PickLang Message String where
    pick lang (Message msg) =
        case lang of
            En -> msg.en
            Es -> msg.es
            Pt -> msg.pt

instance pickLangUrl :: PickLang LangUrl String where
    pick lang (LangUrl url) =
        case lang of
            En -> url.en
            Es -> url.es
            Pt -> url.pt

instance pickLangTalk :: PickLang Talk String where
    pick lang (Talk talk) =
        case lang of
            En -> talk.en
            Es -> talk.es
            Pt -> talk.pt

instance pickVidTime :: PickLang VidTime Timing where
    pick lang (VidTime vt) =
        case lang of
            En -> vt.en
            Es -> vt.es
            Pt -> vt.pt

class PickLoc a b | a -> b where
    pickLoc :: Location -> a -> b

instance pickLocUrl :: PickLoc LocUrl String where
    pickLoc loc (LocUrl lurl) =
        case loc of
            Coimbra -> lurl.coimbra
            Lamego  -> lurl.lamego
            Leon    -> lurl.leon
            Madrid  -> lurl.madrid

instance pickLocLangUrl :: PickLoc LocLangUrl LangUrl where
    pickLoc loc (LocLangUrl llurl) =
        case loc of
            Coimbra -> llurl.coimbra
            Lamego  -> llurl.lamego
            Leon    -> llurl.leon
            Madrid  -> llurl.madrid

instance pickLocHighlight :: PickLoc LocHighlight (Nullable Highlight) where
    pickLoc loc (LocHighlight lhl) =
        case loc of
            Coimbra -> lhl.coimbra
            Lamego  -> lhl.lamego
            Leon    -> lhl.leon
            Madrid  -> lhl.madrid

instance pickLocActive :: PickLoc LocActive Active where
    pickLoc loc (LocActive lact) =
        case loc of
            Coimbra -> lact.coimbra
            Lamego  -> lact.lamego
            Leon    -> lact.leon
            Madrid  -> lact.madrid

instance pickLocTalk :: PickLoc LocTalk Talk where
    pickLoc loc (LocTalk ltalk) =
        case loc of
            Coimbra -> ltalk.coimbra
            Lamego  -> ltalk.lamego
            Leon    -> ltalk.leon
            Madrid  -> ltalk.madrid

instance pickLocVidTime :: PickLoc LocVidTime VidTime where
    pickLoc loc (LocVidTime lvt) =
        case loc of
            Coimbra -> lvt.coimbra
            Lamego  -> lvt.lamego
            Leon    -> lvt.leon
            Madrid  -> lvt.madrid
