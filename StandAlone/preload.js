// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

const {webFrame, remote} = require('electron');
// const console = require('console');

const autoZoom = remote.getGlobal ('autoZoom');

if (autoZoom.isAuto) {
    // console.log ("Preload got zoom value: " + autoZoom.zoom);
    // This seems to have been a misunderstanding: it looks as if it refers
    // to adjustments relative to the initial zoom, which is set in run.js.
    // webFrame.setVisualZoomLevelLimits (autoZoom.zoom, autoZoom.zoom);
    webFrame.setVisualZoomLevelLimits (1, 1);
} else {
    // console.log ("Preload using standard zoom (1)");
    webFrame.setVisualZoomLevelLimits (1, 1);
}


