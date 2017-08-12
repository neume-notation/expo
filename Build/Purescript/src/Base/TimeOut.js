"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

/* Timeout object, designed to be used in a Halogen EventSource.  The time
 * can be reset, for example when the user interacts with the containing
 * component.
 */

function TimeOut () {
    this._timerId = null;
    this._handler = null;
}

TimeOut.prototype.attach = function (handler) { this._handler = handler; };

TimeOut.prototype.fromNow = function (delay) {
    var self = this;
    this._clear();
    this._timerId = setTimeout ( function () {
                                    if (self._handler !== null) {
                                        self._handler ();
                                    }
                                }
                              , delay
                              )
};

TimeOut.prototype.destroy = function () {
    this._clear();
    this._handler = null;
};

TimeOut.prototype._clear = function () {
    if (this._timerId !== null) {
        clearTimeout (this._timerId);
        this._timerId = null;
    }
};

// Create a TimeOut object - this is an effect in Eff
exports.createTimeOut = function () {
    return new TimeOut();
};

// Destroy a TimeOut object - the argument is a reference to a TimeOut object
// previously returned by createTimeOut, and the result is an effect in Eff
exports.destroyTimeOut = function (t) {
    return function () {
        t.destroy();
    };
};

// Register a handler for the timout event.  This is used in a call to
// Halogen.eventSource_
exports.onTimeOut = function (t) {
    return function (handler) {
        return function () {
            t.attach (handler);
        };
    };
};

// Set or reset the timeout n milliseconds from now.  The first argument is
// the TimeOut object.
exports.fromNow = function (t) {
    return function (n) {
        return function () {
            t.fromNow(n);
        };
    };
};
