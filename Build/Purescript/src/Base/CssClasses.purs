-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Base.CssClasses where

import Halogen.HTML                (ClassName(..))

-- .touchclick is used as a jQuery selector for non-button elements which need
-- to be included by a delegated event handler; it modifies (adds to) the
-- generation of click events from touch gestures.
touchclick :: ClassName
touchclick  = ClassName "touchclick"

fullheight :: ClassName
fullheight = ClassName "fullheight"

mag_glass :: ClassName
mag_glass = ClassName "mag-glass"

big_button :: ClassName
big_button = ClassName "big-button"

long_button :: ClassName
long_button = ClassName "long-button"

vlong_button :: ClassName
vlong_button = ClassName "vlong-button"

small_long_button :: ClassName
small_long_button = ClassName "small-long-button"

home_button :: ClassName
home_button = ClassName "home-button"

red_button :: ClassName
red_button = ClassName "red-button"

orange_button :: ClassName
orange_button = ClassName "orange-button"

green_button :: ClassName
green_button = ClassName "green-button"

dark_green_button :: ClassName
dark_green_button = ClassName "dark-green-button"

blue_button :: ClassName
blue_button = ClassName "blue-button"

grey_button :: ClassName
grey_button = ClassName "grey-button"

at_bottom :: ClassName
at_bottom = ClassName "at-bottom"

tight_centre :: ClassName
tight_centre = ClassName "tight-centre"

controls_col :: ClassName
controls_col = ClassName "controls-col"

detail_controls :: ClassName
detail_controls = ClassName "detail-controls"

detail_default :: ClassName
detail_default = ClassName "detail-default"

vbigmargin :: ClassName
vbigmargin = ClassName "vbigmargin"

vseparate :: ClassName
vseparate = ClassName "vseparate"

vpad :: ClassName
vpad = ClassName "vpad"

vcentre :: ClassName
vcentre = ClassName "vcentre"

must_be_top :: ClassName
must_be_top = ClassName "must-be-top"

as_mask :: ClassName
as_mask = ClassName "as-mask"

highlight :: ClassName
highlight = ClassName "highlight"

border_highlight :: ClassName
border_highlight = ClassName "border-highlight"

end_buttons :: ClassName
end_buttons = ClassName "end-buttons"

fadein :: ClassName
fadein = ClassName "fadein"

fadeout :: ClassName
fadeout = ClassName "fadeout"

faded :: ClassName
faded = ClassName "faded"

small_text :: ClassName
small_text = ClassName "small-text"
