"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

var disp = require ("dispatch.js");

// In Eff, so return a function
exports.newDispatcher = function (talkSequence) {
  return function () {
    return new disp.Dispatcher (talkSequence);
  };
};

// Register a callback handler for video events.  Note that this is in Eff,
// so the return value is a function which performs the actual registration.
// The handler itself is in Eff: we need to actually carry out the action,
// ie call the thunk.
exports.onDispatch = function (d) {
  return function (handler) {
    function h (x) {
      return handler(x)();
    }
    return function () {
      d.onDispatch (h);
    };
  };
};
