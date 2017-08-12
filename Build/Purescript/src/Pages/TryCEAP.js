"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

var ceap = require("miniceap.js");

// Two arguments, but still return a function, since the result is in Eff
exports.createCEAP = function (dom) {
  return function (html) {
    return function () {
      return new ceap.EditNeumes (dom, html);
    };
  };
};

// Argument is a reference to the EditNeumes object previously returned by
// a call to createCEAP, result is in Eff
exports.destroyCEAP = function (e) {
  return function () {
    e.unAttach();
  };
};

exports.getChantData = function (e) {
  return function () {
    return e.getChantData();
  };
};

exports.markBadBoxes = function (e) {
  return function (boxes) {
    return function () {
      e.markBadBoxes (boxes);
    }
  };
};
