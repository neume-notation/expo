"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

/* The Dispatcher object has the `talkSequence` built in, and provides two
 * interfaces:
 *
 *     1) The `dispatch` method, needed for the Narrative object.
 *     2) Registering, and calling when necessary, one or more handlers
 *        to receive the processed dispatch information.
 *
 * The `talkSequence` is an array of objects containing at least the
 * properties:
 *
 *     { name  :: String,  -- The name of the step (matching a name in
 *                         -- the timings passed to the Narrative object
 *       reset :: Boolean  -- Whether this is a "reset" step
 *     }
 *
 * In practice, each entry will also contain a property defining the actual
 * action, but it is opaque at this level.
 *
 * The `dispatch` method takes one to three parameters.  If `reset` is
 * omitted it is treated as false, and if `fromTarget` is omitted as well, it
 * is taken as being the same as `toTarget`.  The function acts as follows:
 *
 *     1) If `reset` is true, then find the latest *reset* step in
 *        `talkSequence` at or before the one named `fromTarget`, and
 *        use it as the starting point; otherwise the starting point is
 *        simply the one named `fromTarget`.
 *
 *     2) Loop from the starting point to the endpoint calling
 *        the corresponding action functions (if defined).
 *
 * Note that it is legal for `fromTarget` to be later than `toTarget`:
 * if not `reset`, this simply means that nothing needs to be done, but
 * if `reset` is true, the corresponding reset step might be earlier,
 * resulting in a non-empty range of steps to be carried out.
 *
 * The result of processing is to call the registered handler(s)
 * zero or more times.  The parameter to the handler is an element
 * of the original `talkSequence`, which (presumably) contains the
 * actual action in an additional, opaque property.
 */

function Dispatcher (talkSequence) {

    this.talkSequence = talkSequence;

    // For efficiency keep a mapping from names to indices
    this.talkByName = {};
    for (var i = 0; i < talkSequence.length; i++) {
        this.talkByName[ talkSequence[i].name ] = i;
    }

    // Registered handlers
    this.handlers = [];
};


/* Register a handler
 */
Dispatcher.prototype.onDispatch = function (handler) {
    this.handlers.push (handler);
};


/* The `dispatch` method is what the Narrative object uses
 */
Dispatcher.prototype.dispatch = function (toTarget, fromTarget, reset) {
    var end   = this.talkByName [toTarget],
        from  = fromTarget || toTarget,
        start = this.talkByName [from];
    if (start === undefined || end === undefined) {
        // Debug: uncomment to debug mistakes during development
        // alert ("Undefined step: " + from + " or " + toTarget);
        start = Math.max (start, 0); // not right, just prevent crash
        end   = Math.max (end, 0);
    }
    if (reset) {
        start = backToLastResetStep (this.talkSequence, start);
    }
    // Debug: uncomment to trace behaviour during development
    // console.log ("Dispatcher.dispatch (" + start + ", " + end + ", " + reset
    //              + ") to " + this.handlers.length + " handlers.");
    // Now call the handler(s) to carry out actions from `start` to `end`
    // inclusive.  There is no call if `start` > `end`
    for (var j = start; j <= end; j++) {
        for (var i = 0; i < this.handlers.length; i++) {
            this.handlers[i] ( this.talkSequence[j] );
        }
    }
};


/* Internal function to find the latest step in the sequence which has
 * `reset: true` and is no later in the sequence than the one numbered `start`.
 */
function backToLastResetStep (seq, start) {
    for (var step = start; step >= 0; step--) {
        if (seq[step].reset) {
            return step;
        }
    }
    // Debug: uncomment to debug mistakes during development
    // alert ("No reset step at or before " + start);
    return 0;   // This might be the best we can do, but the fact we got here
                // presumably means it wasn't marked reset, which it should be.
};

exports.Dispatcher = Dispatcher;
