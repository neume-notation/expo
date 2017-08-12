-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Main (main) where

-- Build the html pages for the mobile site in /tmp/MobileWebSite, which
-- should already exist.

import Prelude

import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable               (elem, for_)
import Data.Maybe                  (Maybe(..))
import Data.String                 (Pattern(..), stripSuffix)
import Data.String.Regex           (replace)
import Data.String.Regex.Flags     (noFlags)
import Data.String.Regex.Unsafe    (unsafeRegex)
import Node.Encoding               (Encoding(..))
import Node.FS                     (FS)
import Node.FS.Sync                (readTextFile, writeTextFile)
import Text.Handlebars             (compile)

import Base.Types                  (Location(..), Language(..))
import Base.ExpoData               (Message(..), Talk(..),
                                    expoData, pick, pickLoc)


-- Special messages needed here
titleMsg :: Message
titleMsg = Message {
             en: "Medieval fragments: Old Hispanic chant vestiges"
           , es: "Fragmentos medievales: vestigios del canto hispánico"
           , pt: "Fragmentos medievais: vestígios do canto hispânico"
           }
brandMsg :: Message
brandMsg = Message {
             en: "Medieval fragments"
           , es: "Fragmentos medievales"
           , pt: "Fragmentos medievais"
           }
msAtMsg :: Message
msAtMsg  = Message {
             en: "Manuscript at"
           , es: "Fragmento en"
           , pt: "Manuscrito em"
           }
videoMsg :: Message
videoMsg  = Message {
             en: "Video version"
           , es: "Vídeo"        -- Ideally include some hint, like the English
           , pt: "Vídeo"        -- "version", that this is the *same* thing
                                -- in a different medium
           }
imageMsg :: Message
imageMsg  = Message {
             en: "Photographs"
           , es: "Fotografías"
           , pt: "Fotografias"
           }


type MenuItem = {
      name     :: String
    , active   :: Boolean
    , url      :: String
    }

type MSMenu = {
      msName   :: String
    , active   :: Boolean
    , images   :: MenuItem        -- page of images of this MS
    , codex    :: MenuItem
    , useofms  :: MenuItem
    , textmore :: MenuItem
    , textbits :: MenuItem
    }

type TemplateData = {
      language :: String
    , site     :: { title  :: String
                  , brand  :: String
                  , home   :: String
                  }
    , myself   :: { en :: String   -- URLs for this page in different languages
                  , es :: String
                  , pt :: String
                  }
    , menu     :: { intro  :: MenuItem
                  , music  :: MenuItem
                  , msmenu :: Array MSMenu
                  }
    , content  :: String
    }

type VideoData = { webm :: String, mp4 :: String }

type ImageData = { heading :: String, imageUrls :: Array String }


-- The data for a single page.  The `pageName` is "index", "images" or one of
-- the names handled by `pageContent` below, and pageText is the corresponding
-- body text in all languages.
--
pageData :: Location -> Language -> String -> Talk -> TemplateData
pageData loc lang pageName pageText = {
      language : show lang
    , site     : { title  : pick lang titleMsg
                 , brand  : pick lang brandMsg
                 , home   : urlFor loc lang "index"
                 }
    , myself   : { en : urlFor loc En pageName
                 , es : urlFor loc Es pageName
                 , pt : urlFor loc Pt pageName
                 }
    , menu     : { intro  : { name   : nameFor lang "intro"
                            , active : pageName == "intro"
                            , url    : urlFor loc lang "intro"
                            }
                 , music  : { name   : nameFor lang "music"
                            , active : pageName == "music"
                            , url    : urlFor loc lang "music"
                            }
                 , msmenu : msMenuData loc lang pageName
                                <$> [ Coimbra, Lamego, Leon, Madrid]
                 }
    , content  : addHeading loc lang pageName
                   $ noGreenButton
                   $ pick lang pageText
    }


msMenuData :: Location -> Language -> String -> Location -> MSMenu
msMenuData loc lang pageName itemLoc =
    { msName   : pick lang msAtMsg <> " " <> placeName lang itemLoc
    , active   : loc == itemLoc && elem pageName [ "codex", "useofms", "textmore", "textbits" ]
    , images   : { name : pick lang imageMsg
                 , active : loc == itemLoc && pageName == "images"
                 , url    : urlFor itemLoc lang "images"
                 }
    , codex    : { name : nameFor lang "codex"
                 , active : loc == itemLoc && pageName == "codex"
                 , url    : urlFor itemLoc lang "codex"
                 }
    , useofms  : { name : nameFor lang "useofms"
                 , active : loc == itemLoc && pageName == "useofms"
                 , url    : urlFor itemLoc lang "useofms"
                 }
    , textmore : { name : nameFor lang "textmore"
                 , active : loc == itemLoc && pageName == "textmore"
                 , url    : urlFor itemLoc lang "textmore"
                 }
    , textbits : { name : nameFor lang "textbits"
                 , active : loc == itemLoc && pageName == "textbits"
                 , url    : urlFor itemLoc lang "textbits"
                 }
    }


-- Adjust a TemplateData value to refer to the corresponding video, using
-- the supplied content (which should basically be a <video> tag)
--
adjustForVideo :: TemplateData -> String -> TemplateData
adjustForVideo pData video =
    pData {
          myself  = { en: "vid_" <> pData.myself.en
                    , es: "vid_" <> pData.myself.es
                    , pt: "vid_" <> pData.myself.pt
                    }
        , content = video
    }


-- The `pageName` parameter is one or our short names for talk pages:
-- "intro", "music", "codex", "useofms", "textbits" or "textmore".
-- (The "index" page is handled separately by reading files at the top level,
-- and image and video pages have their own form of content.)
--
pageContent :: Location -> String -> Talk
pageContent loc pageName =
    if pageName == "intro" then pickLoc Madrid expoData.locTalks.introduction
    else if pageName == "music" then expoData.talks.music
    else pickLoc loc (
           if pageName == "codex" then expoData.locTalks.codicology
           else if pageName == "useofms" then expoData.locTalks.useofms
           else if pageName == "textmore" then expoData.locTalks.textmore
           else if pageName == "textbits" then expoData.locTalks.textbits
           else -- Do something, but shouldn't happen!
                expoData.locTalks.introduction
           )


-- The `pageName` is the same as for `pageContent`
--
nameFor :: Language -> String -> String
nameFor lang pageName =
    let msg = if pageName == "intro" then expoData.messages.nav_intro
              else if pageName == "music" then expoData.messages.nav_not_intro
              else if pageName == "codex" then expoData.messages.nav_ms_like
              else if pageName == "useofms" then expoData.messages.nav_ms_usage
              else if pageName == "textmore" then expoData.messages.nav_txt_more
              else if pageName == "textbits" then expoData.messages.nav_txt_bits
              else -- Do something, but shouldn't happen!
                  expoData.messages.nav_intro
    in noStar $ pick lang msg


placeName :: Language -> Location -> String
placeName lang itemLoc =
    pick lang ( case itemLoc of
                    Coimbra -> expoData.messages.nav_alt_coimbra
                    Lamego  -> expoData.messages.nav_alt_lamego
                    Leon    -> expoData.messages.nav_alt_leon
                    Madrid  -> expoData.messages.nav_alt_madrid
              )


placeShort :: Location -> String
placeShort itemLoc =
    case itemLoc of
        Coimbra -> "ca"
        Lamego  -> "lo"
        Leon    -> "l5"
        Madrid  -> "b1"


-- To avoid special cases in relative urls, we just put all of the text pages
-- in a single level of directory.  The `pageName` here includes "index" and
-- "images" in addition to those handled by `pageContent`.
--
urlFor :: Location -> Language -> String -> String
urlFor loc lang pageName =
    let locPage = if pageName == "index" || pageName == "intro" || pageName == "music"
                  then pageName
                  else pageName <> "_" <> placeShort loc
    in
    show lang <> "_" <> locPage <> ".html"


-- Url of the page which displays the video (not for "index")
--
videoUrlFor :: Location -> Language -> String -> String
videoUrlFor loc lang pageName = "vid_" <> urlFor loc lang pageName


-- The actual video media (not for "index")
--
mediaFor :: Location -> Language -> String -> VideoData
mediaFor loc lang pageName =
    let locVideo = if pageName == "intro" then "intro_gc"
                   else if pageName == "music" then "music"
                   else pageName <> "_" <> placeShort loc
    in {
        webm: "static/expo/video/" <> show lang <> "/" <> locVideo <> ".webm"
      , mp4:  "static/expo/videoMp4/" <> show lang <> "/" <> locVideo <> ".mp4"
    }


-- Remove the asterisk from those menu item labels which have them
--
noStar :: String -> String
noStar s =
    case stripSuffix (Pattern "*") s of
        Just t  -> t
        Nothing -> s


-- Remove the section about the green button from the introduction.  This
-- relies on the specific data-id used for this section.
--
noGreenButton :: String -> String
noGreenButton s =
    let re = unsafeRegex
                 "<section data-id=\"intro-codex\">[\\s\\S]*?</section>"
                 noFlags
    in replace re "" s


-- Add a heading to the beginning of the content
--
addHeading :: Location -> Language -> String -> String -> String
addHeading loc lang pageName content =
    if pageName == "index" || pageName == "images" then content
    else
        let item  = nameFor lang pageName
            place = if pageName == "intro" || pageName == "music" then ""
                    else " (" <> placeName lang loc <> ")"
            video = videoUrlFor loc lang pageName
            link  = pick lang videoMsg
        in
        "<h3>" <> item <> place <> "</h3>"
        <> "<p><a href=\"" <> video <> "\">" <> link <> "</a></p>"
        <> content


-- Content of the pages of images for the MS at the location.  The result
-- masquerades as a Talk, but the only difference between the languages
-- is the heading.
--
imageContent :: (ImageData -> String) -> Location -> Talk
imageContent iTemplate loc =
    let images = case loc of
                   Coimbra -> [ pickLoc loc expoData.locUrls.textbits_ms1
                              , pickLoc loc expoData.locUrls.textbits_ms2
                              ]
                   Lamego  -> [ pickLoc loc expoData.locUrls.textbits_ms2
                              , pickLoc loc expoData.locUrls.textbits_ms4
                              ]
                   Leon    -> [ pickLoc loc expoData.locUrls.textbits_ms1
                              , pickLoc loc expoData.locUrls.textbits_ms3
                              ]
                   Madrid  -> [ pickLoc loc expoData.locUrls.textbits_ms1
                              , pickLoc loc expoData.locUrls.textbits_ms2
                              , pickLoc loc expoData.locUrls.textbits_ms3
                              , pickLoc loc expoData.locUrls.textbits_ms4
                              ]
    in Talk { en: doImg images En, es: doImg images Es, pt: doImg images Pt }
  where
    doImg urls lang =
      iTemplate { heading  : placeName lang loc
                , imageUrls: urls
                }


doPage :: forall eff.
               (TemplateData -> String)
            -> (VideoData -> String)
            -> (ImageData -> String)
            -> Location
            -> Language
            -> String
            -> Eff (exception :: EXCEPTION, fs :: FS | eff) Unit
doPage template vTemplate iTemplate loc lang pageName = do
    content <- if pageName == "index" then do
                   en_index <- readTextFile UTF8 "templates/en_index.html"
                   es_index <- readTextFile UTF8 "templates/es_index.html"
                   pt_index <- readTextFile UTF8 "templates/pt_index.html"
                   pure $ Talk { en: en_index, es: es_index, pt: pt_index }
               else if pageName == "images" then
                   pure $ imageContent iTemplate loc
               else
                   pure $ pageContent loc pageName
    let pData    = pageData loc lang pageName content
        result   = template pData
        filename = urlFor loc lang pageName
    writeTextFile UTF8 ("/tmp/MobileWebSite/" <> filename) result

    when (pageName /= "index" && pageName /= "images") do
        let video = vTemplate $ mediaFor loc lang pageName
            vData = adjustForVideo pData video
            vPage = template vData
        writeTextFile UTF8 ("/tmp/MobileWebSite/" <> "vid_" <> filename) vPage


main :: forall eff. Eff (exception :: EXCEPTION, fs :: FS | eff) Unit
main = do
    templateSrc <- readTextFile UTF8 "templates/mobile-template.html"
    vidTemplateSrc <- readTextFile UTF8 "templates/mobile-video.html"
    imageTemplateSrc <- readTextFile UTF8 "templates/mobile-images.html"
    let template  = compile templateSrc
        vTemplate = compile vidTemplateSrc
        iTemplate = compile imageTemplateSrc
        locations = [ Coimbra, Lamego, Leon, Madrid ]
        languages = [ En, Es, Pt ]
        pages     = [ "index", "intro", "music" ]
        locPages  = [ "images", "codex", "useofms", "textmore", "textbits" ]
    for_ languages \lang -> do
        for_ pages \page -> do
            doPage template vTemplate iTemplate Madrid lang page
    for_ locations \loc -> do
        for_ languages \lang -> do
            for_ locPages \page -> do
                doPage template vTemplate iTemplate loc lang page
