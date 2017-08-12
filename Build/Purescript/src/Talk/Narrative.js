"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

var nar = require("narrative.js");

// Three arguments, but still return a function, since the result is in Eff
exports.createNarrative = function (attach) {
  return function (spec) {
    return function (dispatch) {
      return function () {
        return new nar.Narrative (attach, spec, dispatch);
      };
    };
  };
};

// Argument is a reference to the Narrative object previously returned by
// a call to createNarrative, result is in Eff
exports.destroyNarrative = function (n) {
  return function () {
    n.unAttach();
  };
};


// Register a handler for widget-initiated changes.  Note that this is in Eff,
// so the return value is a function which performs the actual registration.
// The handler itself is in Eff: we need to actually carry out the action,
// ie call the thunk.
exports.onWidget = function (n) {
  return function (handler) {
    function h (x) {
      return handler(x)();
    }
    return function () {
      n.onWidget (h);
    };
  };
};

exports.playVideo = function (n) {
  return function () {
    n.playVideo ();
  };
};

exports.pauseVideo = function (n) {
  return function () {
    n.pauseVideo ();
  };
};

exports.hideVideo = function (n) {
  return function () {
    n.hideVideo ();
  };
};

exports.unhideVideo = function (n) {
  return function () {
    n.unhideVideo ();
  };
};

exports.resize = function (n) {
  return function () {
    n.resize ();
  };
};

exports.isVideoHidden = function (n) {
  return function () {
    return n.isVideoHidden();
  };
};

exports.isVideoPlaying = function (n) {
  return function () {
    return n.isVideoPlaying();
  };
};

exports.videoTime = function (n) {
  return function () {
    return n.videoTime();
  };
};

exports.textMidHeight = function (n) {
  return function () {
    return n.textMidHeight();
  };
};
