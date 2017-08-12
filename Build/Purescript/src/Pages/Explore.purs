-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Explore (
    Query,
    ui
) where

-- Two-level menu structure showing all available pages, plus
-- "Back to beginning".
-- All pages are started in their default state, showing a video if one is
-- available, and starting it at the beginning.
--
-- Although superficially different, the credits page actually needs a subset
-- of the same functionality: the short timeout and the lifecycle actions
-- associated with it, and a "home" button together with its help message
-- in all three languages.  For this reason, it is implemented here, using
-- an "options" value which causes the alternative html to be rendered.

import Prelude

import Control.Monad.Eff.JQuery as JQ
import Data.Map                    (empty)
import Data.Maybe                  (Maybe(..))
import Data.Nullable               (toMaybe)

import DOM.HTML as DH
import DOM.HTML.Window as DW

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as HB

import Base.Types                  (Location(..), Language, M)
import Base.Router                 (Routes(..), Extra, setRoute, introRoute,
                                    restartRoute, homeRoute)
import Base.ExpoData               (Message, LangUrl, expoData, pick)
import Base.CssClasses as Class
import Base.TimeOut as TO
import Base.UserPref as U

import Pages.Shared.Types as T
import Pages.Shared.EndButtons     (renderSmallFlags)

myself :: Location -> Language -> Extra -> Routes
myself = Explore


-- Timeout this page after this many milliseconds
maxTime :: Int
maxTime = U.timeout * 1000


data SubMenu = Notation | Codicology | TheText | OtherLoc
derive instance eqSubMenu :: Eq SubMenu

type State = T.PageState (
               submenu  :: Maybe SubMenu
             , timeout  :: Maybe TO.TimeOut
             )

initialState :: T.PageState () -> State
initialState init = {
                 location: init.location
               , language: init.language
               , options:  init.options
               , submenu:  Nothing
               , timeout:  Nothing
               }

data Query a =   OpenSub SubMenu a
               | GoTo Routes a
               | ChangeMS Location a
               | ChangeLang Language a
               | StartTimer a
               | ClearTimer a
               | FeedBack LangUrl a


locName :: Location -> Message
locName x = case x of
              Coimbra -> expoData.messages.nav_alt_coimbra
              Lamego  -> expoData.messages.nav_alt_lamego
              Leon    -> expoData.messages.nav_alt_leon
              Madrid  -> expoData.messages.nav_alt_madrid


doMain :: State
          -> { act :: H.Action Query, msg :: Message }
          -> H.ComponentHTML Query
doMain state x =
    HH.button
    [ HE.onClick $ HE.input_ x.act
      , HP.classes [ Class.vlong_button, Class.blue_button ]
    ]
    [ HH.text $ pick state.language x.msg ]

mainMenu :: State -> Array (H.ComponentHTML Query)
mainMenu state =
    [ HH.button
      [ HE.onClick $ HE.input_
                     (GoTo $ introRoute state.location state.language empty)
      , HP.classes [ Class.vlong_button, Class.blue_button ]
      ]
      [ HH.text $ pick state.language expoData.messages.nav_intro ]
    ]
    <> ( doMain state
         <$> [ { act: OpenSub Codicology, msg: expoData.messages.nav_ms }
             , { act: OpenSub Notation,   msg: expoData.messages.nav_not }
             , { act: OpenSub TheText,    msg: expoData.messages.nav_txt }
             , { act: OpenSub OtherLoc,   msg: expoData.messages.nav_alt }
             ]
       )
  where
    locDflt = case U.location of
                  Just fixed -> fixed
                  Nothing    -> state.location


doSub :: State
         -> { page :: Location -> Language -> Extra -> Routes, msg :: Message }
         -> H.ComponentHTML Query
doSub state x =
    HH.div_
    [ HH.button
      [ HE.onClick
            $ HE.input_ (GoTo $ x.page state.location state.language empty)
      , HP.classes [Class.vlong_button, Class.blue_button]
      ]
      [ HH.text $ pick state.language x.msg ]
    ]

subCodicology :: State -> Array (H.ComponentHTML Query)
subCodicology state =
    doSub state
        <$> [ { page: Codex,    msg: expoData.messages.nav_ms_like }
            , { page: UseOfMS,  msg: expoData.messages.nav_ms_usage }
            , { page: Survival, msg: expoData.messages.nav_ms_survive }
            ]

subNotation :: State -> Array (H.ComponentHTML Query)
subNotation state =
    doSub state
        <$> [ { page: Music,    msg: expoData.messages.nav_not_intro }
            , { page: Cadence,  msg: expoData.messages.nav_not_cadence }
            , { page: Phrases,  msg: expoData.messages.nav_not_phrases }
            , { page: TryCEAP,  msg: expoData.messages.nav_not_ceap }
            ]

subText :: State -> Array (H.ComponentHTML Query)
subText state =
    doSub state
        <$> [ { page: TextMore, msg: expoData.messages.nav_txt_more }
            , { page: TextBits, msg: expoData.messages.nav_txt_bits }
            , { page: ReadText, msg: expoData.messages.nav_txt_read }
            ]

subLocation :: Maybe Location -> State -> Array (H.ComponentHTML Query)
subLocation thisLoc state =
    doSubLoc <$> [ { loc: Coimbra, msg: expoData.messages.nav_alt_coimbra }
                 , { loc: Lamego,  msg: expoData.messages.nav_alt_lamego }
                 , { loc: Leon,    msg: expoData.messages.nav_alt_leon }
                 , { loc: Madrid,  msg: expoData.messages.nav_alt_madrid }
                 ]
  where
    doSubLoc x =
      let m = if Just x.loc == thisLoc
                then expoData.messages.nav_alt_this
                else x.msg
      in
      HH.div_
      [ HH.button
        [ HE.onClick $ HE.input_ (GoTo $ Explore x.loc state.language empty)
        , HP.classes [Class.vlong_button, Class.blue_button]
        ]
        [ HH.text $ pick state.language m ]
      ]


-- All variants of this page have language flags and a "New Visitor" button
-- in a separate well below the main content.
--
bottomWell :: State -> H.ComponentHTML Query
bottomWell state =
    HH.div [ HP.classes [ HB.well, HB.wellLg ] ]
    ( [ renderSmallFlags ChangeLang
      , HH.div_
        [ HH.button
          [ HE.onClick $ HE.input_ (GoTo $ restartRoute state.language)
          , HP.classes [ Class.vlong_button, Class.red_button ]
          ]
          [ HH.strong_
            [ HH.text $ pick state.language expoData.messages.start_again ]
          ]
        ]
      ]
      <> feedbackButton state
    )


-- If feedback URLs are defined (which only happens on the web version) then
-- add a "feedback" button to the bottom well
--
feedbackButton :: State -> Array (H.ComponentHTML Query)
feedbackButton state =
    case toMaybe expoData.feedback of
      Nothing -> []
      Just fbUrl ->
        [ HH.div_
          [ HH.button
            [ HE.onClick $ HE.input_ (FeedBack fbUrl)
            , HP.classes [ Class.vlong_button, Class.dark_green_button ]
            ]
            [ HH.text $ pick state.language expoData.messages.nav_feedback ]
          ]
        ]


-- The main purpose of this component is to display the full menu page
--
renderExplore :: State -> H.ComponentHTML Query
renderExplore state =
    HH.div [ HP.classes [ HB.row, Class.fullheight,
                          HH.ClassName "explore-menu" ] ]
      [ HH.div [ HP.class_ HB.colMd2 ]
        []
      , HH.div [ HP.classes [ HB.colMd8, Class.vcentre ] ]
        [ HH.div [ HP.class_ HB.row ]
          [ HH.div [ HP.class_ HB.colMd12 ]
            [ HH.div [ HP.classes [ HB.well, HB.wellLg ] ]
              [ HH.h2_
                [ HH.text $
                    pick state.language expoData.messages.nav_head
                    <> " "
                    <> pick state.language (locName state.location)
                ]
              , HH.p_
                [ HH.text $ pick state.language expoData.messages.nav_greet ]
              ]
            ]
          ]
        , HH.div [ HP.classes [ HB.row, HH.ClassName "explore-boxes" ] ]
          [ HH.div [ HP.classes [ HB.colMd6, HH.ClassName "explore-main" ] ]
            [ HH.div [ HP.classes [ HB.well, HB.wellLg ] ]
              $ mainMenu state
            ]
          , HH.div [ HP.classes [ HB.colMd6, HH.ClassName "explore-sub"] ]
            [ HH.div [ HP.classes [ HB.well, HB.wellLg ] ]
              case state.submenu of
                Just Notation   -> subNotation state
                Just Codicology -> subCodicology state
                Just TheText    -> subText state
                Just OtherLoc   -> subLocation U.location state
                Nothing         -> []
            ]
          ]
        , HH.div [ HP.class_ HB.row ]
          [ HH.div [ HP.class_ HB.colMd12 ]
            [ bottomWell state ]
          ]
        ]
      , HH.div [ HP.class_ HB.colMd2 ]
        []
      ]


-- The secondary purpose of this component is to display the credits
--
renderCredits :: State -> H.ComponentHTML Query
renderCredits state =
    HH.div [ HP.classes [ HB.row, Class.fullheight,
                          HH.ClassName "explore-menu" ] ]
      [ HH.div [ HP.class_ HB.colMd2 ]
        []
      , HH.div [ HP.classes [ HB.colMd8, Class.vcentre ] ]
        [ HH.div [ HP.classes [ HB.well, HB.wellLg ] ]
          [ HH.h2_
            [ HH.text $ pick state.language expoData.messages.credits_head ]
          -- Actual html embedded by initializer, using jQuery
          , HH.div [ HP.class_ $ HH.ClassName "embed-credits" ] []
          ]
        , bottomWell state
        ]
      , HH.div [ HP.class_ HB.colMd2 ]
        []
      ]


ui :: forall eff. H.Component HH.HTML Query (T.PageState ()) Void (M eff)
ui = H.lifecycleComponent {
       initialState: initialState
     , render
     , eval
     , receiver: const Nothing
     , initializer: Just $ H.action StartTimer
     , finalizer: Just $ H.action ClearTimer
     }
  where
    render :: State -> H.ComponentHTML Query
    render state = if T.hasOption state T.optCredits then renderCredits state
                   else renderExplore state

    eval :: Query ~> H.ComponentDSL State Query Void (M eff)
    eval (StartTimer next) = do
      timer <- H.liftEff TO.createTimeOut
      H.subscribe $  H.eventSource_ (TO.onTimeOut timer) $
          GoTo homeRoute H.Listening
      H.liftEff $ TO.fromNow timer maxTime
      H.modify (\s -> s {timeout = Just timer})
      evalEmbedCredits
      pure next
    eval (ClearTimer next) = do
      mtimer <- H.gets _.timeout
      H.modify (_ { timeout = Nothing } )         -- Remove object reference
      case mtimer of
        Just timer -> H.liftEff $ TO.destroyTimeOut timer
        Nothing    -> pure unit
      pure next
    eval (OpenSub submenu next) = do
      -- Clicking/tapping the *same* top-level item toggles its state, but
      -- clicking a new one always opens that new one, implicitly closing
      -- any old one
      evalUpdateTimeOut
      old <- H.gets _.submenu
      let newSubmenu = if old == Just submenu then Nothing
                       else Just submenu
      H.modify (\s -> s {submenu = newSubmenu})
      pure next
    eval (GoTo route next) = do
      H.liftEff $ setRoute route
      pure next
    eval (ChangeMS loc next) = do
      evalUpdateTimeOut
      H.modify (\s -> s {location = loc})
      pure next
    eval (ChangeLang lang next) = do
      evalUpdateTimeOut
      H.modify (\s -> s {language = lang})
      evalEmbedCredits  -- New html for the modified language (now in the state)
      pure next
    eval (FeedBack fbUrl next) = do
      lang <- H.gets _.language
      win <- H.liftEff DH.window
      let url      = pick lang fbUrl
          name     = "_blank"
          features = ""
      _ <- H.liftEff $ DW.open url name features win
      pure next


-- FIXME This seems quite precarious, but it does work.  Especially when called
-- from ChangeLang, it is presumably modifying the old DOM, which is about to
-- be updated, and gets away with it because the new VDOM doesn't differ at
-- this point, so virtual-dom leaves it in place.  A better solution would
-- be nice!
evalEmbedCredits :: forall eff. H.ComponentDSL State Query Void (M eff) Unit
evalEmbedCredits = do
    state <- H.get
    let html = pick state.language expoData.talks.credits
    when (T.hasOption state T.optCredits) $
        H.liftEff $ JQ.select ".embed-credits" >>= JQ.setHtml html

evalUpdateTimeOut :: forall eff. H.ComponentDSL State Query Void (M eff) Unit
evalUpdateTimeOut = do
      mtimer <- H.gets _.timeout
      case mtimer of
        Just timer -> H.liftEff $ TO.fromNow timer maxTime
        Nothing    -> pure unit
