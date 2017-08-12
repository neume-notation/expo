-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Handler.MiniCEAP (
    miniCEAP,
    similarNeumes,
    transcription
) where

import Import          hiding (joinPath)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet                   (hamletFile)


-- Data for the menu (table) of neume images.
--
-- Each row consists of a melodic reading and a list of neumes.  The neumes
-- are each defined by a Route to the image, an id number (which agrees with
-- the route, actually) and three integer metrics.

data Neume = Neume { neumeUrl    :: Route App
                   , neumeId     :: Int
                   , neumeBBLeft :: Int
                   , neumeWidth  :: Int
                   , neumeHeight :: Int
                   }

-- This is just the required subset of Leon5 neumes
--
neumeMenu :: [ ( Text, [ Neume ] ) ]
neumeMenu = [
      ( "N",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r786_svg, neumeId = 786
              , neumeBBLeft = 5, neumeWidth = 20, neumeHeight = 21
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r787_svg, neumeId = 787
              , neumeBBLeft = 5, neumeWidth = 23, neumeHeight = 22
              }
          ]
      )

    , ( "N",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r788_svg, neumeId = 788
              , neumeBBLeft = 5, neumeWidth = 26, neumeHeight = 36
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r789_svg, neumeId = 789
              , neumeBBLeft = 5, neumeWidth = 32, neumeHeight = 51
              }
          ]
      )

    , ( "NL",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r791_svg, neumeId = 791
              , neumeBBLeft = 5, neumeWidth = 26, neumeHeight = 37
                }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r792_svg, neumeId = 792
              , neumeBBLeft = 5, neumeWidth = 32, neumeHeight = 33
              }
          ]
      )

    , ( "NH",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r796_svg, neumeId = 796
              , neumeBBLeft = 5, neumeWidth = 33, neumeHeight = 88
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r797_svg, neumeId = 797
              , neumeBBLeft = 5, neumeWidth = 26, neumeHeight = 55
              }
          ]
      )

    , ( "NHL",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r801_svg, neumeId = 801
              , neumeBBLeft = 5, neumeWidth = 36, neumeHeight = 67
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r802_svg, neumeId = 802
              , neumeBBLeft = 5, neumeWidth = 38, neumeHeight = 71
              }
          ]
      )

    , ( "NHH",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r804_svg, neumeId = 804
              , neumeBBLeft = 5, neumeWidth = 58, neumeHeight = 71
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r816_svg, neumeId = 816
              , neumeBBLeft = 5, neumeWidth = 50, neumeHeight = 86
              }
          ]
      )

    , ( "NHHH",
          [ Neume {
                neumeUrl = StaticR expo_img_Leon5_r812_svg, neumeId = 812
              , neumeBBLeft = 5, neumeWidth = 72, neumeHeight = 81
              }
          , Neume {
                neumeUrl = StaticR expo_img_Leon5_r811_svg, neumeId = 811
              , neumeBBLeft = 5, neumeWidth = 50, neumeHeight = 116
              }
          ]
      )
    ]


-- These neumes are considered "similar" when scoring, so having the "wrong"
-- one of a group is less serious that totally the wrong shape.
--
similarNeumes :: [ [ Text ] ]
similarNeumes =
    [ ["786", "787"]
    , ["788", "789"]
    , ["791", "792"]
    , ["801", "801"]
    , ["811", "812"]
    ]


-- This is the text we are using
--
textBox :: Text
textBox = "In hoc cog-no-vi quo-ni-am vo-lu-is-ti me qui-a"

newtype NeumePlaced = NeumePlaced (Text, Int, Int)
instance ToJSON NeumePlaced where
    toJSON (NeumePlaced (nid, x, y)) =
        object ["neume" .= nid, "x" .= x, "y" .= y]

newtype ChantSyl = ChantSyl (Int, Text, [NeumePlaced])
instance ToJSON ChantSyl where
    toJSON (ChantSyl (ident, txt, neumes)) =
        object ["ident" .= ident, "txt" .= txt, "neumes" .= neumes]

-- Here is the "official" transcription as a list of ChantSyl
--
transcription :: [ ChantSyl ]
transcription = [
      ChantSyl (0,  "In",   [ NeumePlaced ("816",7,24) ] )
    , ChantSyl (1,  "hoc",  [ NeumePlaced ("796",6,43) ] )
    , ChantSyl (2,  "cog-", [ NeumePlaced ("811",7,13) ] )
    , ChantSyl (3,  "no-",  [ NeumePlaced ("801",3,65) ] )
    , ChantSyl (4,  "vi",   [ NeumePlaced ("791",8,89) ] )
    , ChantSyl (5,  "quo-", [ NeumePlaced ("786",12,104) ] )
    , ChantSyl (6,  "ni-",  [ NeumePlaced ("786",11,104) ] )
    , ChantSyl (7,  "am",   [ NeumePlaced ("796",4,41)
                            , NeumePlaced ("786",35,80)
                            ] )
    , ChantSyl (8,  "vo-",  [ NeumePlaced ("786",9,108) ] )
    , ChantSyl (9,  "lu-",  [ NeumePlaced ("787",11,98) ] )
    , ChantSyl (10, "is-",  [ NeumePlaced ("789",8,65)
                            , NeumePlaced ("792",24,31)
                            , NeumePlaced ("812",45,40)
                            , NeumePlaced ("792",98,14)
                            ] )
    , ChantSyl (11, "ti",   [ NeumePlaced ("804",5,50) ] )
    , ChantSyl (12, "me",   [ NeumePlaced ("796",7,37) ] )
    , ChantSyl (13, "qui-", [ NeumePlaced ("802",4,53) ] )
    , ChantSyl (14, "a",    [ NeumePlaced ("786",12,104) ] )
    ]


-- This is not a top level Handler, but rather a fuction in the Handler monad
-- called from getExpoR
miniCEAP :: Handler LText
miniCEAP = do

    urlRender <- getUrlRenderParams

    let ceapUrl  = $(hamletFile "templates/expo/miniceap.hamlet")
        ceapHtml = renderHtml $ ceapUrl urlRender

    return ceapHtml
