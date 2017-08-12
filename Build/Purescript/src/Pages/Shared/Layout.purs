-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Pages.Shared.Layout (
    renderTwoCol
) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria
import Halogen.Themes.Bootstrap3 as HB

import Base.CssClasses as Class

-- This is the layout used on most of our pages, where the left hand side
-- is usually for illustration, and the right hand side for the main focus
-- of the activity or presentation.
--
-- The two parameters give the content of the two sides.
--
renderTwoCol :: forall p i. Array (H.HTML p i) -> Array (H.HTML p i) -> H.HTML p i
renderTwoCol lhs rhs = 
    HH.div
        [ HP.classes [ HB.row, Class.fullheight ] ]
        [ HH.div
          [ HP.classes [ HB.colMd6, Class.fullheight ] ]
          lhs
        , HH.div
          [ HP.classes [ HB.colMd6, Class.fullheight ], Aria.role "main" ]
          rhs
        ]
