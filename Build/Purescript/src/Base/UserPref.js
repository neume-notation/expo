"use strict";

// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

var guess =
    (typeof standAloneLang !== 'undefined') ? standAloneLang
    : ( (typeof window !== 'undefined') ? (
                window.navigator.browserLanguage
             || window.navigator.userLanguage
             || window.navigator.language
             || "en"
         ) :
             "en"
      );

exports.language = guess.toLowerCase().substr(0,2);

exports.locationString = (typeof standAloneLoc !== 'undefined')
                         ? standAloneLoc : "";

exports.timeout = (typeof standAloneFast !== 'undefined') ? standAloneFast : 120;
