// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

/* This script may be moved into layout-wrapper once it is working fully.
 *
 * This is used only when running stand-alone, and its effect is to simplify
 * the response to touch gestures.  Users unfamiliar with touch devices seem
 * to be inclined to use multiple fingers on large buttons, or to press too
 * deliberately, resulting in a long touch.  We do not need multitouch or
 * any special interpretation of long touches, so this code attempts to
 * translate them to ordinary click events.
 *
 * The idea is that some touch events created deferred clicks, but these are
 * cancelled if something else results in a click within a short time interval.
 * Conversely, a click masks the special touch handling for a similarly short
 * interval.  Therefore the additional click event is suppressed if something
 * else generates a click within a short time either side of the touch event,
 * for example, if the browser itself generates a click from the touch.
 *
 * The clicks generated here are *additional* to anything the browser
 * might do, so should never make matters worse.
 */

var deferred = null,
    masked   = null;

function touchHandler (event) {
    if (masked === null && deferred === null) {
        // Do not do anything special with this event if
        //   - a recent click is masking it
        //   - another recent touch event has already initated a deferred click
        deferred = setTimeout ( function () {
                                    deferred = null;
                                    $(event.currentTarget).click();
                                },
                                100   // 1/10 second delay
                              );
    }
}


function clickHandler (event) {
    if (deferred !== null) {
        clearTimeout (deferred);
        deferred = null;
    }
    if (masked === null) {
        masked = setTimeout (function () { masked = null; }, 100);
    }
}

/* Only apply this logic to buttons, and to other elements marked explicitly
 * with the class .touchclick.  These are the elements which have significant
 * behaviour on clicks, so, although `deferred` and `masked` are single
 * variables, the simulated click events will occur on the right elements
 * without being masked by events bubbling through other elements.
 *
 * Attach the delegated event handlers to the body, waiting for the document
 * to be ready, because jquery.mCustomScrollbar attaches a handler to the
 * document itself which, for unidentified reasons, interferes with the
 * ones we are attaching.
 */
$( function() {

    $("body").on ("click", "button, .touchclick", clickHandler);
    $("body").on ("touchend", "button, .touchclick", touchHandler);

    // This seems to confuse some things, and doesn't work so well anyway
    //$(document).on ("touchstart", "button, .touchclick", touchHandler);

    // Stop contextmenu wherever it occurs
    $("body").on ("contextmenu", function (e) { e.preventDefault(); } );

    // Select needs to be stopped, but preventing the default on selectstart
    // seems to interfere with touch scrolling.  Instead 'user-select: none;'
    // in the CSS seems to work better.
    //$("body").on ("selectstart", function (e) { e.preventDefault(); } );

    // On the *descendents* of these specific elements, attempt to prevent
    // default touch actions completely.
    $("body").on ("touchstart", "button *, .touchclick *",
                    function (e) { e.preventDefault(); } );
    $("body").on ("touchend", "button *, .touchclick *",
                    function (e) { e.preventDefault(); } );

    // These do not seem to help
    // $(document).on ("dragstart", "button *, .touchclick *",
    //                 function (e) { e.preventDefault(); } );
    // $(document).on ("copy", "button *, .touchclick *",
    //                 function (e) { e.preventDefault(); } );
});
