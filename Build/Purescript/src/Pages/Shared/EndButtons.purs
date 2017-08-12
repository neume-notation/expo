-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Shared.EndButtons (
    RouteSpec,
    renderEndButtons,
    renderEndButtonsOr,
    renderSmallFlags,
    evalGoTo,
    evalGoToExtra,
    evalResume
) where

-- Helpers for `render` on almost every page.  `renderEndButtons` shows
-- three buttons, one to return to the very beginning, one to continue on
-- the path we suggest, and one to go to the general Explore page.  Variants
-- of this function make it conditional on *not* having returnTo specified,
-- and, in the case of `renderEndButtonsOr` to add a "back" button.
--
-- Also there is an accompanying `eval` helpers for a `GoTo route` action,
-- which is very likely to be needed in a component which uses these buttons,
-- and for a `Resume` action which is likely to be needed for a "back" button.
--
-- Note that the caller must have a state type consistent with PageState
-- from Pages.Shared.Types or, for some functions, PageState.

import Prelude

import Control.Monad.Eff.Class     (class MonadEff)
import Control.Monad.State.Class   (class MonadState)
import Data.Either                 (Either(..))
import Data.Map                    (empty)
import Data.Maybe                  (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Base.Types                  (Location, Language(..), dfltLang,
                                    ExpoEffects)
import Base.Router                 (Routes(..), Extra, restartRoute, setRoute)
import Base.ExpoData               (expoData, pick)
import Base.CssClasses as Class
import Base.UserPref as U

import Pages.Shared.Types as T


-- The action is passed a value of this type.  If a Right value, it is a
-- complete route, but if a Left value, the caller still needs to provide
-- the Extra.  It is intended that the value will be passed back into
-- evalGoTo, or to evalGoToExtra with an Extra value to use in the cases
-- which need it.
type RouteSpec = Either (Extra -> Routes) Routes

-- `act` is an action in the query algebra of the caller, and it expects a
-- parameter of type RouteSpec; it should normally call `evalGoTo`, defined
-- below, to change the route.  `cont` is an optional Routes constructor
-- for the next page in the standard sequence.  There are three cases for
-- how the continue button operates, depending on whether `cont` is Nothing,
-- and whether "pos" appears in state.options (meaning that there is a return
-- position in the introduction):
--
--   1) "pos" NOT present: Continue always goes to the Explore menu
--   2) "pos" present, `cont` == Nothing: Continue returns to the position
--       identified by "pos" in the introduction.
--   3) "pos" present, `cont` defines a route: Continue goes to the route
--      defined by `cont`, with the location, language, and extra defined
--      in `state`
--
-- The `myself` parameter is the Routes contructor of the calling page,
-- used to contruct the correct Routes when changing the language.
--
-- Layout is mainly determined by the `at_bottom` class, which expects
-- to be in a `fullheight` container, with button separation being controlled
-- by the `end_buttons` class.
--
-- Rationale: it is tempting to use styled <a> tags instead of buttons for
-- situations like this, and let the browser change the route for us so that
-- the Halogen component doesn't need to do anything directly.  However,
-- browsers tend to display the location if we do that, which is certainly
-- not what we want in a UI, as opposed to a web page!
--
renderEndButtons :: forall f r p.
                       (RouteSpec -> H.Action f)
                    -> (Location -> Language -> Extra -> Routes)
                    -> Maybe (Location -> Language -> Extra -> Routes)
                    -> T.PageState r
                    -> Array (H.HTML p f)
renderEndButtons act myself cont state =
    let loc  = state.location
        lang = state.language
        next = case state `T.option` T.optIntroPos of
                 Just pos -> case cont of
                               Just c  -> Left $ c loc lang
                               Nothing -> Right $ Intro loc lang state.options
                 Nothing  -> Right $ Explore loc lang empty
        icon = case state `T.option` T.optIntroPos of
                 Just _  -> HH.ClassName "cont-forward"
                 Nothing -> HH.ClassName "cont-menu"
    in
    [ HH.div
      [ HP.classes [Class.at_bottom, Class.end_buttons] ]
      [ HH.button
        [ HE.onClick $ HE.input_ (act $ Right $ restartRoute lang)
        , HP.classes [Class.long_button, Class.red_button]
        ]
        [ HH.strong_ [ HH.text $ pick lang expoData.messages.start_again ] ]
      , renderSmallFlags (\lg -> act $ Right $ myself loc lg state.options)
      , HH.button
        [ HE.onClick $ HE.input_ (act next)
        , HP.classes [Class.long_button, Class.blue_button]
        ]
        [ HH.div [ HP.class_ icon ]
          [ HH.text $ pick lang expoData.messages.continue ]
        ]
      ]
    ]


-- Like `renderEndButtons` except that it renders the back button itself,
-- when it is needed, instead of assuming that it is being rendered by
-- a Narrative.  The `back` parameter is the action to invoke when the
-- back button is clicked.
--
-- In common with `renderEndButtons`, the layout is done assuming that
-- it is in the right-hand half of a two-column layout produced by
-- Layout.renderTwoCol,  Something more sophisticated would be required
-- (wrapping `renderEndButtons` too) for any other division of the page.
--
renderEndButtonsOr :: forall f r p.
                       (RouteSpec -> H.Action f)
                    -> (Location -> Language -> Extra -> Routes)
                    -> Maybe (Location -> Language -> Extra -> Routes)
                    -> H.Action f
                    -> T.PageState r
                    -> Array (H.HTML p f)
renderEndButtonsOr act myself cont back state =
    if not (state `T.hasOption` T.optIntroPos) then
        renderEndButtons act myself cont state
    else
        let btnClasses = case state.language of
                           En -> [ Class.dark_green_button ]
                           _  -> [ Class.dark_green_button, Class.small_text ]
        in
        renderEndButtons act myself cont state
        <>
        [ HH.div [ HP.class_ $ HH.ClassName "end_back_intro" ]
          [ HH.div [ HP.class_ Class.tight_centre ]
            [ HH.button
              [ HE.onClick (HE.input_ back)
              , HP.classes btnClasses
              ]
              [ HH.text $ pick state.language expoData.messages.back_button
              ]
            ]
          ]
        ]


renderSmallFlags :: forall f p.
                       (Language -> H.Action f)
                    -> H.HTML p f
renderSmallFlags langAct =
    HH.div
      [ HP.class_ $ HH.ClassName "small-lang" ]
      ( -- This is the same choice of order as in Start.purs
        if (fromMaybe Es $ dfltLang <$> U.location) == Pt
        then [ smallFlag Pt, smallFlag Es, smallFlag En ]
        else [ smallFlag Es, smallFlag Pt, smallFlag En ]
      )
  where
    smallFlag lang =
      HH.img [ HP.src $ flagImage lang, HE.onClick $ HE.input_ $ langAct lang ]
    flagImage lang = case lang of
                       En -> expoData.urls.small_flag_en
                       Es -> expoData.urls.small_flag_es
                       Pt -> expoData.urls.small_flag_pt


-- The type parameter hm is a HalogenM with some choices of type parameters.
-- All that matters for this function is that it is a MonadEff instance with
-- the ExpoEffects.
evalGoTo :: forall eff n hm. MonadEff (ExpoEffects eff) hm
            => RouteSpec -> n -> hm n
evalGoTo = evalGoToExtra empty

evalGoToExtra :: forall eff n hm. MonadEff (ExpoEffects eff) hm
                 => Extra -> RouteSpec -> n -> hm n
evalGoToExtra extra routeSpec next = do
    let route = case routeSpec of
                    Left incomplete -> incomplete extra
                    Right complete  -> complete
    H.liftEff $ setRoute route
    pure next

evalResume ::  forall eff r n hm. MonadEff (ExpoEffects eff) hm
               => MonadState (T.PageState r) hm
               => n -> hm n
evalResume next = do
    s <- H.get
    case s `T.option` T.optIntroPos of
      Just pos -> do
        -- "pos" is always be set in options if we need to return to the Intro,
        -- and we simply need to pass our options on (, including, for example,
        -- the "hidden" state).
        H.liftEff $ setRoute $ Intro s.location s.language s.options
      Nothing ->
        -- Unused unless the "back" button is reinstated for all cases
        H.liftEff $ setRoute $ Explore s.location s.language empty
    pure next
