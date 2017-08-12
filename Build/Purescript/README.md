Structure
---------

The javascript part of the exhibition software is structured as a single-page
application in which the top-level components are written in Purescript,
using purescript-halogen and purescript-routing.

However, a central part of the exhibition design is a widget which
coordinates the video and scrollable text, and which drives the other
parts of the display.  This component, the "narrative", is rather involved,
and includes third-party jQuery libraries for the scrolling (jQuery custom
content scroller and jQuery Mouse Wheel Plugin).  It seemed easiest to
write the whole "narrative" widget in javascript plus jQuery, and run it
from Halogen as a third-party component.

The narrative widget takes a callback function which is responsible for
dispatching to actions.  Some of these actions may affect the video (for
example pausing it), but most will update the DOM managed by Halogen (eg
highlights).  The dispatcher is also a javascript object.

Assumptions
-----------

The Purescript code relies on a very few things being set up separately:

  - To make buttons lose focus after a click, to avoid a dotted
    outline remaining, a jQuery delegated event handler is set up in
    `layout-wrapper.hamlet`.  I cannot find a way to do this from Halogen,
    but a single, global handler is probably cleaner anyway!

  - A function to return to the locally-configured start page is defined
    in layout-wrapper.hamlet and used in `Router.js`.  It works by setting
    the hash part of the URL, which triggers Halogen in our arrangement.

  - Optionally a global inactivity timer is set up in `layout-wrapper.hamlet`.
    It calls the function just mentioned to return to the start page.

Building
--------

The purescript in this hierarchy results in a single entry point (`main`)
which sets up everything needed.  It is therefore compiled to a single
result file, using a very simple build process:

    bower install   # Only if starting from a clean state (see below)
    npm run build

`dist/expo.min.js` should then be copied (or linked during development) to
static/expo/js

Note that the script "build" is defined in `package.json` to be:

    NODE_PATH="$(pwd)/src/JSModules:$NODE_PATH" \
        pulp browserify --optimise --to dist/expo.js
    minify dist/expo.js

The setting of NODE_PATH gives us a place to put Common JS modules which are
not directly FFI modules. The narrative widget is a good example.  Such
modules are placed under src/JSModules, and browserify needs to know about
this.

Clean State
-----------

A clean state can be achieved by running

    npm run clean

This completely removes

  - `bower_components`
  - `node_modules`
  - `output`
  - `.psci_modules`
  - `.pulp-cache`
  - `dist/expo.js`
  - `dist/expo.min.js`

After this, `bower install` will (re)create `bower_components` and
`node_modules`, and install the correct dependencies.  Then `npm run build`,
`pulp browserify`, or any other `pulp` subcommand which results in compilation,
will (re)create `output` (and .pulp-cache).
