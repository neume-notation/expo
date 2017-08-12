#!/bin/bash

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# This script builds a temporary hierarchy containing a minimal Yesod
# server to serve the exhibition, runs it, gets the expo page, and edits
# it to be correct for the stand-alone single-page application.
#
# There are two variants, depending on whether an option "-w" or "--website"
# is given: if it is, the `standAlone` variable in the template is left False,
# the WebSite hierarchy is linked in for static files, and mp4 video files
# are referenced from the data templates.  It still produces a single-page
# application which can be served statically, but it is configured for the
# web version rather than the installed version.
#
# This is all intended only as part of the build process for the stand-alone
# exhibition, for which it has the advantage over the CEAP integration that
# it allows Yesod to check the presence of static files in exactly the
# hierarchy we intend to distribute.  However, the server code might be useful
# as a starting point for a production server which is independent of CEAP,
# but it would need careful review and cleaning up of redundant code.  
#
# Run this from the top level of the "Expo" hierarchy.

SNAPSHOT="lts-8.24"

OPTS="$*"

EXPO=$(pwd)

if [[ $OPTS =~ -w|--website ]]; then
    STATIC=$EXPO/WebSite/full/static
else
    STATIC=$EXPO/StandAlone/static
fi

pushd /tmp

# Create new temporary project
# ----------------------------

cp $EXPO/Build/Scripts/yesod-simple.hsfiles .
stack --resolver $SNAPSHOT new MiniWebSite ./yesod-simple

cd MiniWebSite

# Remove pieces we do not want polluting the hierarchy
# ----------------------------------------------------

rm Handler/Comment.hs
rm Handler/Home.hs
rm templates/*
rm -rf static

# Create links to the Expo hierarchy
# ----------------------------------

ln -s $EXPO Expo

mkdir Layout
cd Layout
ln -s ../Expo/CEAPIntegration/Layout/Expo.hs Expo.hs

# Specific version of Layout.ExpoIsStandAlone
if [[ $OPTS =~ -w|--website ]]; then
  cp ../Expo/CEAPIntegration/Layout/ExpoIsStandAlone.hs ExpoIsStandAlone.hs
else
  sed -e 's/False/True/' < ../Expo/CEAPIntegration/Layout/ExpoIsStandAlone.hs \
      > ExpoIsStandAlone.hs
fi

cd ../Handler
ln -s ../Expo/CEAPIntegration/Handler/Expo.hs Expo.hs
ln -s ../Expo/CEAPIntegration/Handler/MiniCEAP.hs MiniCEAP.hs

# Copy templates, and modify the video ones to remove mp4 entries
cd ../templates
cp -a ../Expo/templates expo
if ! [[ $OPTS =~ -w|--website ]]; then
    (cd expo/data
        for file in videos.julius locVideos.julius; do
            mv $file $file.orig
            $EXPO/Build/Scripts/noMp4.py $file.orig > $file
        done
    )
fi

# Use the actual static hierarchy for the stand-alone app
cd ..
ln -s $STATIC static

# Patch or replace a few files in the scaffolding
# -----------------------------------------------

# Application.hs
#
patch -p0 <<'EOF'
--- Application.hs.orig	2017-01-06 14:08:39.941895000 +0000
+++ Application.hs	2017-01-06 14:48:27.665895000 +0000
@@ -30,8 +30,7 @@
 -- Import all relevant handler modules here.
 -- Don't forget to add new modules to your cabal file!
 import Handler.Common
-import Handler.Home
-import Handler.Comment
+import Handler.Expo
 
 -- This line actually creates our YesodDispatch instance. It is the second half
 -- of the call to mkYesodData which occurs in Foundation.hs. Please see the
EOF

# Foundation.hs
# Just remove unnessecary stuff, replacing defaultLayout with a dummy
#
patch -p0 <<'EOF'
--- Foundation.hs.orig	2017-01-06 14:08:39.949895000 +0000
+++ Foundation.hs	2017-01-06 15:41:10.442612627 +0000
@@ -72,40 +72,7 @@
     -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
     yesodMiddleware = defaultYesodMiddleware
 
-    defaultLayout widget = do
-        master <- getYesod
-        mmsg <- getMessage
-
-        mcurrentRoute <- getCurrentRoute
-
-        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
-        (title, parents) <- breadcrumbs
-
-        -- Define the menu items of the header.
-        let menuItems =
-                [ NavbarLeft $ MenuItem
-                    { menuItemLabel = "Home"
-                    , menuItemRoute = HomeR
-                    , menuItemAccessCallback = True
-                    }
-                ]
-
-        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
-        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
-
-        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
-        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
-
-        -- We break up the default layout into two components:
-        -- default-layout is the contents of the body tag, and
-        -- default-layout-wrapper is the entire page. Since the final
-        -- value passed to hamletToRepHtml cannot be a widget, this allows
-        -- you to use normal widget features in default-layout.
-
-        pc <- widgetToPageContent $ do
-            addStylesheet $ StaticR css_bootstrap_css
-            $(widgetFile "default-layout")
-        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
+    defaultLayout _ = return "Dummy"  -- Markup (Html) has an IsString instance
 
     -- Routes not requiring authenitcation.
     isAuthorized FaviconR _ = return Authorized
@@ -142,9 +109,9 @@
     makeLogger = return . appLogger
 
 -- Define breadcrumbs.
-instance YesodBreadcrumbs App where
-  breadcrumb HomeR = return ("Home", Nothing)
-  breadcrumb  _ = return ("home", Nothing)
+-- instance YesodBreadcrumbs App where
+--  breadcrumb HomeR = return ("Home", Nothing)
+--  breadcrumb  _ = return ("home", Nothing)
 
 -- This instance is required to use forms. You can modify renderMessage to
 -- achieve customized and internationalized form validation messages.
EOF

# config/routes
#
cat > config/routes <<'EOF'
/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/            ExpoR GET
/count/#Text TrackR GET
EOF

# The .cabal file
#
patch -b -p0 << 'EOF'
--- MiniWebSite.cabal.orig	2017-05-12 10:19:28.458922179 +0100
+++ MiniWebSite.cabal	2017-05-12 11:08:05.037617658 +0100
@@ -20,14 +20,16 @@
                      Settings
                      Settings.StaticFiles
                      Handler.Common
-                     Handler.Home
-                     Handler.Comment
+                     Handler.Expo
+                     Handler.MiniCEAP
+                     Layout.Expo
+                     Layout.ExpoIsStandAlone
 
     if flag(dev) || flag(library-only)
         cpp-options:   -DDEVELOPMENT
         ghc-options:   -Wall -fwarn-tabs -O0
     else
-        ghc-options:   -Wall -fwarn-tabs -O2
+        ghc-options:   -Wall -fwarn-tabs -O0
 
     extensions: TemplateHaskell
                 QuasiQuotes
@@ -87,6 +89,9 @@
                  , time
                  , case-insensitive
                  , wai
+                 , filepath
+                 , word8
+                 , blaze-html
 
 executable         MiniWebSite
     if flag(library-only)
EOF

# Now build and run it
# --------------------

if ! stack build; then
    echo "The build of the MiniWebSite failed"
    exit 1
fi

killall -q MiniWebSite
LOG=$(tempfile)
stack exec MiniWebSite >& $LOG &

sleep 20

retries=10
while ! wget --no-proxy -O /tmp/index.html.orig http://localhost:3000/; do
    if (( retries == 0 )); then
        echo "The MiniWebSite server is not responding"
        killall MiniWebSite
        exit 1
    fi
    retries=$(( $retries - 1 ))
    sleep 10
done

cat $LOG

killall MiniWebSite

# For the static web site, make the tracker url relative, otherwise leave it
# alone, since we have a trivial localhost server in the standalone version.
if [[ $OPTS =~ -w|--website ]]; then
    TRACKER='s!http://localhost:3000(?=/count/)!..!;'
else
    TRACKER=';'
fi
# The second pattern is a very specific one in expoStartPage ()
perl -pe 's!http://localhost:3000/static/!static/!g;'        \
     -e 's!http://localhost:3000/(?=#" \+ homeHash)!!;'      \
     -e $TRACKER                                             \
     -e 's!\?etag=[^"\\]*!!g'                                \
     /tmp/index.html.orig                                    \
   > /tmp/index.html

popd
