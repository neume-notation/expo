#!/bin/bash

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# This pulls together the build steps, with each one invoked only if its
# result seems to be outdated (unless an argument is given).  Some steps
# are commented out, and are included mainly as a record of what needs
# to be done.
#
# The following global things need to have been done:
#
#     - tools noted in Build/PureScript/README installed
#     - `cleancss` installed
#     - `stack` installed from fp-complete respository
#     - bootstrap built using the `variables.less` file from Build/CSS

# The script may be given optional arguments to force building of some or all
# components, regardless of whether they appear to be up to date, and, in some
# cases, to invoke additional cleaning prior to building.  Any out of date
# components will still be built in any case.  The recognised values are:
#
#     all    Force rebuild of all components, in some cases from a clean state
#     css    Force rebuild of the merged css (but not Bootstrap)
#     purs   Rebuild the Purescript, *without* prior cleaning
#     spa    Build the MiniWebServer and generate the single-page index.html
#     web    Build the MiniWebServer in the web site configuration, and
#            generate the web version of the single-page index.html
#
# Any number of these may be given, but there is no point combining anything
# else with `all` since it includes the others.
#
NO_OPT="$*"

dirname=${0%/*}
if [ -d $dirname ]; then
    tools=$(cd $dirname; pwd)
else
    tools=`pwd`
fi
cd $tools/../..


# Unpack and copy the relevant parts of jQuery custom content scroller and
# jQuery Mouse Wheel Plugin, as in README:
#
# (   cd /tmp
#     unzip path/to/jquery-mousewheel-master.zip
#     tar xzf path/to/malihu-custom-scrollbar-plugin-3.1.5.tar.gz
# )
# cp -p /tmp/jquery-mousewheel-master/jquery.mousewheel.min.js static/js/
# cp -p /tmp/malihu-custom-scrollbar-plugin-3.1.5/mCSB_buttons.png static/css/
# cp -p /tmp/malihu-custom-scrollbar-plugin-3.1.5/jquery.mCustomScrollbar.css Build/CSS/
# cp -p /tmp/malihu-custom-scrollbar-plugin-3.1.5/jquery.mCustomScrollbar.js static/js
# (   cd static/js
#     minify jquery.mCustomScrollbar.js
# )

# CSS is built according to the instructions in Build/CSS/README.
#
if ! [[ -f Build/CSS/bootstrap.css ]] || [[ $NO_OPT =~ all ]] \
   || [[ Build/CSS/variables.less -nt Build/CSS/bootstrap.css ]]; then
    # The full build involes compiling a customised Bootstrap, but is
    # commented out for now:
    #
    # (   cd /tmp
    #     unzip path/to/bootstrap-3.3.7.zip
    # )
    # cp Build/CSS/variables.less /tmp/bootstrap-3.3.7/less/variables.less
    # (   cd /tmp/bootstrap-3.3.7
    #     npm install
    #     grunt dist
    # )
    # cp -p /tmp/bootstrap-3.3.7/dist/css/bootstrap.css Build/CSS/
    # cp -p /tmp/bootstrap-3.3.7/dist/fonts/* static/fonts/
    # cp -p /tmp/bootstrap-3.3.7/dist/js/bootstrap.min.js static/js/
    #
    echo "Building customised Bootstrap"
    if ! [[ $NO_OPT =~ all ]]; then
        echo "Bootstrap needs to be rebuilt but cannot (yet) be done by this script"
        exit 1
    else
        # Don't prevent an "all" build continuing
        echo "    ... skipped"
    fi
fi
if ! [[ -f Build/CSS/merged.min.css ]] || [[ $NO_OPT =~ all|css ]] \
   || [[ -n $(find Build/CSS/* -newer Build/CSS/merged.min.css) ]]; then
    echo "Building combined, minified CSS"
    (   cd Build/CSS
        cat bootstrap.css expo.css miniceap.css jquery.mCustomScrollbar.css \
            | cleancss > merged.min.css
        cat bootstrap.css expo.css smallscreen.css | cleancss > small.min.css
    )
fi

# The Purescript is built using `npm` - detailed instructions are in
# Build/Purescript/README
#
if ! [[ -f Build/Purescript/dist/expo.min.js ]] || [[ $NO_OPT =~ all|purs ]] \
   || [[ -n $(find Build/Purescript/src -newer Build/Purescript/dist/expo.min.js) ]]; then
    echo "Building Purescript"
    (   cd Build/Purescript
        if [[ $NO_OPT =~ all ]]; then
            # Clean first and do full build if NO_OPT includes "all"
            npm run clean
        fi
        if ! [[ -d bower_components ]]; then
            bower install
        fi
        npm run build
    )
fi

# Generate the JSON messages file from the XML master
#
if ! [[ -f templates/data/messages.json ]] || [[ $NO_OPT =~ all ]] \
   || [[ templates/data/messages.xml -nt templates/data/messages.json ]]; then
    echo "Building messages.json"
    (   cd templates/data
        ../../Build/Scripts/xmlmessages.py messages.xml > messages.json
    )
fi

# Create the index page of the stand-alone application
# The test for newer *static* files does *not* follow symlinks since it is only
# the existence of files which matters here, and this part of the build is
# unnecessary if only file content changes (eg CSS or Purescript).  Including
# `static` as well as `StandAlone/static` handles the one case where a link
# points to a directory (StandAlone/static/expo -> static).
#
if ! [[ -f StandAlone/index.html ]] || [[ $NO_OPT =~ all|spa ]] \
   || [[ -n $(find StandAlone/static static templates -newer StandAlone/index.html) ]]; then
    echo "Building index.html for stand-alone app"
    rm -rf /tmp/MiniWebSite
    ./Build/Scripts/makeSinglePage.sh
    cp -p /tmp/index.html StandAlone/index.html
fi

# The same for the web-site version
#
if ! [[ -f WebSite/index.html ]] || [[ $NO_OPT =~ all|web ]] \
   || [[ -n $(find WebSite/static static templates -newer WebSite/index.html) ]]; then
    echo "Building index.html for web site"
    rm -rf /tmp/MiniWebSite
    ./Build/Scripts/makeSinglePage.sh --website
    cp -p /tmp/index.html WebSite/full/index.html

    echo "Building mobile web pages"
    (   cd Build/Scripts/Mobile
        if [[ $NO_OPT =~ all ]]; then
            # Clean first and do full build if NO_OPT includes "all"
            npm run clean
        fi
        if ! [[ -d node_modules ]]; then
            npm install --production
        fi
    )
    rm -rf /tmp/MobileWebSite
    ./Build/Scripts/makeMobileSite.sh
    cp -p /tmp/MobileWebSite/*.html WebSite/small/
fi

# Windows installer script
#
if ! [[ -f Build/Installer/expo-install.nsi ]] || [[ $NO_OPT =~ all ]] \
   || [[ Build/Installer/Main.hs -nt Build/Installer/expo-install.nsi ]]; then
    echo "Building NSIS script"
    (   cd Build/Installer
        stack build
        stack exec generate-expo-installer
    )
fi

echo ""
echo "You may now wish to build the installer on a Windows machine - see the"
echo "steps *after* generating the .nsi file in Build/Installer/README."
echo ""
