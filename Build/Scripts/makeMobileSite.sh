#!/bin/bash

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# The mobile pages are built using the Purescript program in the subdirectory
# Build/Scripts/Mobile, using the template `templates/mobile-template.html`
# in that directory.
# 
# In order to do this, we use the expoData from the built StandAlone
# hierarchy, for which we need a modified version of a couple of modules
# from Build/Purescript 
#
#     - Base/Types.purs with the dependency on Halogen removed
#     - Base/ExpoData.purs as is, but with a different ExpoData.js
#     - A new ExpoData.js constructed from StandAlone/index.html

EXPO=$(pwd)
IDX=StandAlone/index.html

sed -e '/ExpoEffects/d'                               \
    -e '/DispatchAction/d'                            \
    -e '/Talk\(Element\|Sequence\)/d'                 \
    -e '/^ *M$/d'                                     \
    -e 's/\(Timing(\.\.)\),/\1/'                      \
    -e '/import Control.Monad.Aff/d'                  \
    -e '/import DOM.HTML.Types/d'                     \
    -e '/import Halogen/d'                            \
    -e '/import Network.HTTP.Affjax/d'                \
    Build/Purescript/src/Base/Types.purs              \
    > Build/Scripts/Mobile/src/Base/Types.purs

# First use json2purs.py to create the data.  This is a good check that the
# JSON generated in index.html matches the type declared in ExpoData, but the
# output of string data is more prone to errors, so a simpler method is
# used as well as a check.
#
rm -f Build/Scripts/Mobile/src/Base/ExpoData.js

sed -e '/import Base.Types/s/Timing/Timing(..)/'                              \
    -e '/import Data.Nullable/s/)/, toNullable)/'                             \
    -e '/import Data.Nullable/aimport Data.Maybe                  (Maybe(..))'\
    -e '/foreign import expoData/d'                                           \
    Build/Purescript/src/Base/ExpoData.purs                                   \
    > Build/Scripts/Mobile/src/Base/ExpoData.purs

echo ""                >> Build/Scripts/Mobile/src/Base/ExpoData.purs
echo "expoData :: All" >> Build/Scripts/Mobile/src/Base/ExpoData.purs
echo -n "expoData ="   >> Build/Scripts/Mobile/src/Base/ExpoData.purs

Build/Scripts/json2purs.py $IDX >> Build/Scripts/Mobile/src/Base/ExpoData.purs

rm -rf /tmp/MobileWebSite
rm -rf /tmp/MobileWebSite.check
mkdir /tmp/MobileWebSite

( cd Build/Scripts/Mobile
  npm run html
)

mv /tmp/MobileWebSite /tmp/MobileWebSite.check

( cd /tmp/MobileWebSite.check
  md5sum *.html > /tmp/MobileCheckSums
)

# Now repeat with the simpler method to be sure that string content is right.
# What we have done above is an extremely useful check of the types, though!

cp Build/Purescript/src/Base/ExpoData.purs \
   Build/Scripts/Mobile/src/Base/ExpoData.purs

cat > Build/Scripts/Mobile/src/Base/ExpoData.js <<'EOF'
"use strict";

EOF
tail -n +$(egrep -n 'var expoData' $IDX | cut -d: -f1) $IDX    \
    | sed -e '1s/<script>var /exports./' -e '/<\/script>/,$d'  \
    >> Build/Scripts/Mobile/src/Base/ExpoData.js

mkdir /tmp/MobileWebSite

( cd Build/Scripts/Mobile
  npm run html
)

echo "Nothing should be reported between this line"
( cd /tmp/MobileWebSite
  md5sum -c /tmp/MobileCheckSums | egrep -v ': OK$'
)
echo "and this one!"
