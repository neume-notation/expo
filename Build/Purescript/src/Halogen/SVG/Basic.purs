-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Halogen.SVG.Basic (
    svg,
    path,
    text,
    width,
    height,
    x,
    y,
    viewBox,
    class_,
    fill,
    d,
    strokeWidth,
    strokeLinejoin,
    strokeLinecap,
    textAnchor,
    fontSize
) where

import Halogen.HTML as HH


svgNS :: HH.Namespace
svgNS = HH.Namespace "http://www.w3.org/2000/svg"

-- It seems that we need the full namespace url on elements, not just a
-- prefix.
svgElement :: forall r p i.
                   String
                -> Array (HH.IProp r i)
                -> Array (HH.HTML p i)
                -> HH.HTML p i
svgElement tag = HH.elementNS svgNS (HH.ElemName tag)

-- Use as svgAttr name value
-- It seems that the attributes should not have a namespace at all.  Using
-- a full URL results in an error (from virtual-dom, I think), and a prefix
-- seems to stop it working.
svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name = HH.attr (HH.AttrName name)

-- SVG Elements

svg :: forall r p i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
svg = svgElement "svg"

path :: forall r p i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
path = svgElement "path"

text :: forall r p i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
text = svgElement "text"

-- SVG Attributes

width :: forall r i. String -> HH.IProp r i
width = svgAttr "width"

height :: forall r i. String -> HH.IProp r i
height = svgAttr "height"

x :: forall r i. String -> HH.IProp r i
x = svgAttr "x"

y :: forall r i. String -> HH.IProp r i
y = svgAttr "y"

viewBox :: forall r i. String -> HH.IProp r i
viewBox = svgAttr "viewBox"

class_ :: forall r i. String -> HH.IProp r i
class_ = svgAttr "class"

fill :: forall r i. String -> HH.IProp r i
fill = svgAttr "fill"

d :: forall r i. String -> HH.IProp r i
d = svgAttr "d"

strokeWidth :: forall r i. String -> HH.IProp r i
strokeWidth = svgAttr "stroke-width"

strokeLinejoin :: forall r i. String -> HH.IProp r i
strokeLinejoin = svgAttr "stroke-linejoin"

strokeLinecap :: forall r i. String -> HH.IProp r i
strokeLinecap = svgAttr "stroke-linecap"

textAnchor :: forall r i. String -> HH.IProp r i
textAnchor = svgAttr "text-anchor"

fontSize :: forall r i. String -> HH.IProp r i
fontSize = svgAttr "font-size"
