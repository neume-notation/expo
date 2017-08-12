-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Layout.Expo (
    expoLayout
) where

import ClassyPrelude.Yesod
import Foundation
import Settings
import Settings.StaticFiles
import Text.Hamlet              (hamletFile)
import Layout.ExpoIsStandAlone  (standAlone)


-- Layout for the exhibition presentation.
--
-- Almost all of the HTML is generated by the client-side Purescript code,
-- and so the prupose.  However, a significant contribution from the
-- server is the data used to provide the URLs for resources such as images
-- and videos, and the text and translations for everything from small
-- messages and button labels to the text of talks.  This data is generated
-- using a javascript-only widget, which is combined with the `layout`
-- widget (which merely inserts a script tag into the *body* to include
-- the purescript code).
--
expoLayout :: Widget -> Handler Html
expoLayout dataWidget = do
    master <- getYesod

    -- This is the normal scaffolding split between "expo/layout", which can
    -- contain normal widget features, and "expo/layout-wrapper.hamlet",
    -- which has to be a plain hamlet file.

    pc <- widgetToPageContent $ dataWidget >> $(widgetFile "expo/layout")
    withUrlRenderer $(hamletFile "templates/expo/layout-wrapper.hamlet")
