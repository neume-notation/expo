-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Start (
    State(..),
    initialState,
    Query(..),
    Msg(..),
    ui
) where

import Prelude

import Data.Map                    (empty)
import Data.Maybe                  (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Halogen.SVG.Basic as S

import Base.Types                  (Location(..), Language(..), dfltLang)
import Base.Router                 (Routes(Intro, Explore, Credits))
import Base.ExpoData               (Message, expoData, pick, pickLoc)
import Base.CssClasses as Css
import Base.UserPref as U

import Pages.Shared.Types as T

-- This SVG, and the button in which it is placed, both require the expo CSS
-- There is an ugly hack to adjust the font size for languages which have an
-- overly long translation of "Start"!
startButton :: forall p i. Language -> String -> HH.HTML p i
startButton lang msg =
  let fontAdjust = case lang of
                       Es -> 3
                       Pt -> 2
                       _  -> 0
      txtSize    = show $ 30 - fontAdjust * 3
      txtPosY    = show $ 85 - fontAdjust
  in
  S.svg
    [ S.viewBox "0 0 150 150" ]
    [ S.path
        [ S.class_ "button-arrow right-arrow", S.fill "none"
        , S.strokeWidth "8"
        , S.strokeLinejoin "round", S.strokeLinecap "round"
        , S.d "M 20 40 H 75 V 10 L 140 75 L 75 140 V 110 H 20"
        ]
        []
    , S.text
        [ S.x "15", S.y txtPosY, S.textAnchor "left", S.fontSize txtSize ]
        [ HH.text msg ]
    ]

menuButton :: forall p i. String -> HH.HTML p i
menuButton msg =
  S.svg
    [ S.viewBox "0 0 150 150" ]
    [ S.path
        [ S.fill "none"
        , S.strokeWidth "8"
        , S.strokeLinecap "round"
        , S.d "M 50 33 H 100 M 50 51 H 100 M 50 69 H 100"
        ]
        []
    , S.text
        [ S.x "75", S.y "120", S.textAnchor "middle", S.fontSize "30" ]
        [ HH.text msg ]
    ]

-- The `options` in PageState may include the key `choose` to cause the
-- location chooser to be shown
type State = T.PageState (
               chosen :: Boolean     -- True when location actively chosen
             )

initialState :: T.PageState () -> State
initialState init = {
                 location: init.location
               , language: init.language
               , options:  init.options
               , chosen:   false
               }

data Query a =   SetLang Language a
               -- | SetLoc Location a -- unused at present
               | Start Routes a

data Msg = SetRoute Routes

-- Generate a button to set the langauge
langButton :: String -> String -> String -> H.Action Query
              -> Array (H.ComponentHTML Query)
langButton id_ imageUrl caption action =
    [ HH.button
      [ HE.onClick (HE.input_ action)
      , HP.id_ id_
      , HP.class_ $ HH.ClassName "lang-flag"
      ]
      [ HH.img [ HP.src imageUrl
               , HP.width 150
               , HP.height 100
               ]
      ]
    , HH.div_ [ HH.text caption ]
    ]

-- Button and caption to set Spanish as the language
langSpanish :: Array (H.ComponentHTML Query)
langSpanish =
    langButton "lang-spanish" expoData.urls.flag_spain "Español" (SetLang Es)

-- Button and caption to set Portuguese as the language
langPortuguese :: Array (H.ComponentHTML Query)
langPortuguese =
    langButton "lang-portuguese" expoData.urls.flag_portugal "Português" (SetLang Pt)

-- Button and caption to set English as the language
langEnglish :: Array (H.ComponentHTML Query)
langEnglish =
    langButton "lang-english" expoData.urls.flag_uk "English" (SetLang En)

-- Sort the buttons so that the local language for the place with the
-- exhibit is listed first.  The statically-determined location is used,
-- otherwise the order would flip as the manuscript is chosen on the general
-- start page, and the default is Spanish.
langButtonOrder :: State -> { first  :: Array (H.ComponentHTML Query)
                            , second :: Array (H.ComponentHTML Query)
                            , third  :: Array (H.ComponentHTML Query)
                            }
langButtonOrder state =
    if (fromMaybe Es $ dfltLang <$> U.location) == Pt
    then { first: langPortuguese, second: langSpanish, third: langEnglish }
    else { first: langSpanish, second: langPortuguese, third: langEnglish }

-- "Button" (clickable div with image) to choose location
locButton :: State -> Message -> Location -> H.ComponentHTML Query
locButton state caption loc =
  HH.div
    [ HP.class_ HB.colMd6 ]
    [ HH.div
      [ HP.class_ Css.tight_centre ]
      [ HH.div
        [ -- HE.onClick $ HE.input_ (SetLoc loc)
          HE.onClick $ HE.input_ (Start $ Intro loc state.language empty)
        , HP.classes ( [ HH.ClassName "loc-choice", Css.touchclick ]
                       <> isSelected
                     )
        ]
        [ HH.img [ HP.src $ pickLoc loc expoData.locUrls.choose_loc
                 , HP.width 350
                 , HP.height 200
                 ]
        , HH.div_ [ HH.text $ pick state.language caption ]
        ]
      ]
    ]
  where
    isSelected =
        if state.chosen && state.location == loc
        then [ HH.ClassName "loc-choice-active" ]
        else []

-- Generate "buttons" to choose the location
locChoose :: State -> H.ComponentHTML Query
locChoose state =
    HH.div_
      [ HH.div
        [ HP.class_ HB.row ]
        [ locButton state expoData.messages.alt_coimbra Coimbra
        , locButton state expoData.messages.alt_lamego Lamego
        ]
      , HH.div
        [ HP.classes [ HB.row, Css.vseparate ] ]
        [ locButton state expoData.messages.alt_leon Leon
        , locButton state expoData.messages.alt_madrid Madrid
        ]
      ]

-- Generate the site-dependent magnifying glass
magGlass :: State -> H.ComponentHTML Query
magGlass state =
    HH.div [ HP.class_ Css.mag_glass ]
    [ HH.img [ HP.class_ Css.mag_glass
             , HP.src $ pickLoc state.location expoData.locUrls.mag_glass
             ]
    ]

-- Generate site-dependent logos.  The site here is the static one set by the
-- installation, not anything dynamic
--
logos :: forall p i. HH.HTML p i
logos =
    let localLogo =
            case U.location of
                Just Coimbra  -> expoData.urls.logo_coimbra
                Just Lamego   -> expoData.urls.logo_lamego
                Just Leon     -> expoData.urls.logo_leon
                Just Madrid   -> expoData.urls.logo_bne
                Nothing -> if U.locationString == "Valencia"
                             then expoData.urls.logo_valencia
                             else ""
    in
    HH.div [ HP.class_ Css.tight_centre ]
      ( [ HH.img [ HP.src expoData.urls.logo_bristol ] ]
        <> (if localLogo /= "" then [ HH.img [ HP.src localLogo ] ] else [])
        <> [ HH.img [ HP.src expoData.urls.logo_ahrc ] ]
      )


ui :: forall m. H.Component HH.HTML Query (T.PageState ()) Msg m
ui = H.component {
       initialState: initialState
     , render
     , eval
     , receiver: const Nothing
     }
  where
    render :: State -> H.ComponentHTML Query
    render state =
      let choose = state `T.hasOption` "choose"
          heading = if choose
                    then expoData.messages.greet_head_alt
                    else expoData.messages.greet_head
          message = if choose
                    then expoData.messages.greeting_alt
                    else expoData.messages.greeting
          flags   = langButtonOrder state
      in
      HH.div [ HP.classes [ HB.row, Css.fullheight ] ]
        [ HH.div [ HP.classes [ HB.colMd2, Css.fullheight ] ]
          [ HH.div [ HP.class_ Css.vbigmargin ]
            [ HH.div [ HP.class_ Css.tight_centre ]
              [ HH.div_
                  flags.first
              , HH.div [ HP.class_ Css.vseparate ]
                  flags.second
              , HH.div [ HP.class_ Css.vseparate ]
                  flags.third
              ]
            ]
          , HH.div [ HP.class_ Css.at_bottom ]
            [ HH.div [ HP.class_ Css.tight_centre ]
              [ HH.button
                [ HE.onClick (HE.input_ $ Start $ Explore state.location
                                                          state.language
                                                          empty
                             )
                , HP.classes [Css.dark_green_button, Css.big_button]
                ]
                [ menuButton $ pick state.language expoData.messages.menu ]
              ]
            ]
          ]
        , HH.div [ HP.classes [ HB.colMd8, Css.fullheight ] ]
          [ HH.div [ HP.id_ "greeting"]
            [ HH.h2_
              [ HH.text $ pick state.language heading ]
            , HH.p_
              [ HH.text $ pick state.language message ]
            ]
          , if choose
            then locChoose state
            else magGlass state
          , HH.div [ HP.class_ $ HH.ClassName "logo-box" ]
            [ logos
            , HH.div
              [ HE.onClick (HE.input_ $ Start $ Credits state.location
                                                        state.language
                                                        empty
                           )
              , HP.classes [ HH.ClassName "as-link", Css.tight_centre ]
              ]
              [ HH.text $ pick state.language expoData.messages.credits ]
            ]
          ]
        , HH.div [ HP.classes [ HB.colMd2, Css.fullheight ] ]
          [ HH.div [ HP.class_ Css.at_bottom ]
            [ HH.div [ HP.class_ Css.tight_centre ]
              [ HH.button
                [ HE.onClick (HE.input_ $ Start $ Intro state.location
                                                        state.language
                                                        empty
                             )
                , HP.classes [Css.blue_button, Css.big_button]
                ]
                [ startButton state.language
                              $ pick state.language expoData.messages.start
                ]
              ]
            ]
          ]
        ]

    eval :: Query ~> H.ComponentDSL State Query Msg m
    eval (SetLang lang next) = do
      H.modify (\s -> s {language = lang})
      pure next
    -- SetLoc is unused at present, as location selection does immediate Start
    -- eval (SetLoc loc next) = do
    --   H.modify (\s -> s {chosen = true, location = loc})
    --   pure next
    -- Start simply sends a message to the parent to change the route.  The
    -- alternative would be to put this component explcitly in Aff and call
    -- setRoute here.
    eval (Start route next) = do
      H.raise $ SetRoute route
      pure next
