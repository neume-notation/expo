"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

/* A very much simplified version of the drag-and-drop editing of CEAP,
 * treated as a "third-party" component in Halogen.
 *
 * If `html` is not defined, then `dom` should be the DOM object at the
 * root of the DOM tree for the miniceap html, otherwise `dom` should be
 * an empty DOM element and the html in `html` will inserted into its
 * contents.
 */

function EditNeumes (dom, html) {
    // Variable used to throttle resize events
    this._windowResizeTimer = null;

    // This will be a resize handler
    this.resizeHandler = null;

    this._dom = $(dom);
    if (typeof html != 'undefined') {
        this._dom.html (html);
    }
    this._initialise ();
}


/* Remove the HTML from the DOM.  The resize handler we added to the window
 * object needs to be removed, and if ever we were to attach dropped neumes
 * outside the local tree, cleaning them up would be important.
 */
EditNeumes.prototype.unAttach = function () {
    $(window).off ("resize", this.resizeHandler);
    this._dom.empty ();
}


/* Get the chantdata as a structure of the type Array ChantSyl as defined in
 * ExpoData.purs.  The syllables are sorted by `ident` (all values from 0 to
 * the maximum should be used, in fact), and the neumes within them are sorted
 * by `x` coordinate.
 */
EditNeumes.prototype.getChantData = function () {
    var self = this;
    var chantData = [];
    self._outer.find (".chantsyllable").each (
        function () {
            var ident = $(this).attr ("data-ident") - 0,
                txt   = $(this).children(".chantsyllabletxt").text();
            chantData[ident] = { ident: ident, txt: txt, neumes: [] };
        }
    );

    // Just in case we were missing any (we shouldn't be)
    for (var i = 0; i < chantData.length; i++) {
        if (typeof chantData[i] == 'undefined') {
            chantData[i] = { ident: i, txt: "", neumes: [] };
        }
    }

    // Neumes dragged to boxes are attached directly to self._outer
    self._outer.children (".neumeimage[data-csyl]").each (
        function () {
            var syllable = $(this).attr ("data-csyl") - 0,
                neume    = $(this).attr ("data-id"),
                nleft    = $(this).attr ("data-left") - 0,
                ntop     = $(this).attr ("data-top") - 0;
            chantData[syllable].neumes.push ({neume: neume, x: nleft, y: ntop});
        }
    );

    for (var i = 0; i < chantData.length; i++) {
        chantData[i].neumes.sort (function (a, b) { return a.x - b.x; } );
    }

    return chantData;
};


/* Mark, by changing the background, a set of the boxes, and clear the mark
 * for any which are not in the given list.  The background is put on the
 * .neumeborder div.  The elements of `boxes` are `data-ident` values
 * for the .chantsyllable divs (see also the comment at `badBoxes` in
 * TryCEAP.purs).
 */
EditNeumes.prototype.markBadBoxes = function (boxes) {
    this._outer.find (".neumeborder").removeClass ("marked-bad");
    for (var i = 0; i < boxes.length; i++) {
        this._outer
            .find (".chantsyllable[data-ident='" + boxes[i] + "']>.neumeborder")
            .addClass ("marked-bad");
    }
};


EditNeumes.prototype._initialise = function () {

    var self = this;

    // This is where dragged neumes are attached, and relative to which they
    // are positioned.
    self._outer = self._dom.find ( ".ceap-outer" );

    self._dom.find ( ".neumeimage" ).draggable (
        { helper: "clone"
        , revert: "invalid"
        , cursorAt: { bottom: 6, left: 5 }
        , start: function (event, ui) {
                     $(ui.helper).css ("background-color", "transparent")
                                 .css ("border", "none");
                 }
        }
    );

    self._dom.find ( ".ceap-col1" ).droppable (
        { tolerance: "fit"    // Be fussy for deletion - it's a big area anyway
        , drop: function (event, ui) { $(ui.helper).remove(); }
        }
    );

    self._dom.find ( ".neumedrop" ).droppable ({
        // Full CEAP uses "pointer", but be more fussy to help expo users avoid
        // odd layout; the strictness is eased visually by having the border
        // drawn somewhat inside the .neumedrop (see the .neumeborder class).
        tolerance: "fit",
        drop: function (event, ui) {
            // jQuery UI seems to think it knows about the helper, so we need
            // to make our own clone to keep, yet we still need to remove the
            // helper!
            var $neume = $(ui.draggable).clone(),
                outerPos = self._outer.offset(),
                dragPos = ui.offset,
                thisPos = $(this).offset();
            $(ui.helper).remove();
            $neume.appendTo (self._outer)
                  .css ("position", "absolute")
                  .css ("left", dragPos.left - outerPos.left)
                  .css ("top", dragPos.top - outerPos.top);
            $neume.draggable (
                { helper: "original"
                , revert: "invalid"
                }
            );
            // Record on the element itself, for later use, the ident of the
            // containing syllable, and the position (rounded to integers)
            // relative to the neumedrop.
            $neume.attr (
                { "data-csyl": $(this).closest (".chantsyllable")
                                      .attr ("data-ident")
                , "data-left": Math.round (dragPos.left - thisPos.left)
                , "data-top":  Math.round (dragPos.top - thisPos.top)
                }
            );
        }
    });

    self.resizeHandler = function () { self._windowResize (self); };
    $(window).resize (self.resizeHandler);

}


/* This object has a handler for window resize events because it needs to
 * re-position neumes relative their .neumedrop elements.
 * Throttled by timer to 5 per second
 */
EditNeumes.prototype._windowResize = function (self) {
    if (! self._windowResizeTimer) {
        // Do actions only if timer has finished and removed itself
        self._windowResizeTimer = setTimeout (function () {
                self._position_neumes();
                self._windowResizeTimer = null;  // Remove timer so we can act again
            },
            200   // 1/5 second delay
        );
    }
};


/* Reposition neumes using the neumedrop-relative offsets stored in
 * data-x attributes
 */
EditNeumes.prototype._position_neumes = function () {
    var self = this;
    self._outer.children (".neumeimage[data-csyl]").each (
        function () {
            var outerPos = self._outer.offset(),
                syllable = $(this).attr ("data-csyl"),
                nleft    = $(this).attr ("data-left") - 0,
                ntop     = $(this).attr ("data-top") - 0;
            var dropPos
                  = self._outer
                        .find (".chantsyllable[data-ident='" + syllable + "']")
                        .children (".neumedrop").offset();
            $(this).css ("left", dropPos.left + nleft - outerPos.left)
                   .css ("top",  dropPos.top + ntop - outerPos.top);
        }
    )
}


// This can be used for testing with stand-alone html
//$( function () {
//    var dom = $( ".ceap-outer" ).get(0);
//    var en = new EditNeumes (dom);
//});

exports.EditNeumes = EditNeumes;
