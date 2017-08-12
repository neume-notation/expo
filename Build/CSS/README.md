This is for Bootstrap 3.3.7
===========================

The files here are

  - `variables.less` - bootstrap customisation (goes in bootstrap-3.3.7/less)
  - `bootstrap.css`  - copied from the resulting build (under dist/css)
  - `jquery.mCustomScrollbar.css` - vanilla css from the "jQuery custom
                       content scroller" plugin
  - `expo.css`       - extra CSS for this application
  - `miniceap.css`   - CSS specifically for the TryCEAP eaxample - see the
                       comment in the file about potential for future conflicts

The css files are merged and minified using the node "cleancss" package,
installed from Ubuntu:

    cat bootstrap.css expo.css miniceap.css jquery.mCustomScrollbar.css \
        | cleancss > merged.min.css

A slightly different combination is used for small-screen devices:

    cat bootstrap.css expo.css smallscreen.css | cleancss > small.min.css

Copy these things to the static hierarchy:

    merged.min.css                            to static/expo/css/
    bootstrap-3.3.7/dist/fonts/*              to static/expo/fonts/
    bootstrap-3.3.7/dist/js/bootstrap.min.js  to static/expo/js/
