// Author: Paul Rouse
// Copyright 2017 University of Bristol
// Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

// Modules needed for a trivial http server to do logging for counting purposes
const http = require('http'),
      fs   = require('fs'),
      path = require('path');

const HTTP_PORT = 3000;

// These two should agree with the media queries in the expo CSS
const MIN_SCREEN_WIDTH  = 1680,
      MIN_SCREEN_HEIGHT = 1049;

// Global reference to the trivial http server we start, and the log file
var httpServer = null,
    logFile    = null;

var logLinePos = 0,
    logLastTime = 0;

// This is passed to the "preload" script, and may be modified by calculateZoom
global.autoZoom = { isAuto: false, zoom: 1 };

// Modules to control application life and to create native browser window.
// Ensure here that no proxy is used.
const electron = require('electron')
const {app, BrowserWindow} = electron;
app.commandLine.appendSwitch('--no-proxy-server');

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is GCed.
var mainWindow = null;

// Quit when all windows are closed.
// TODO Decide whether we need this: we are full screen in kiosk mode with
//      no menus, but does Alt-F4 bring us in this way?
app.on('window-all-closed', function() {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform != 'darwin') {
    app.quit();
  }
});

app.on ('will-quit', function () {
  // Try to cleanly shut down the server (but can't do anything if it fails).
  if (httpServer !== null) {
    httpServer.close ();
    httpServer = null;
  }
  if (logFile !== null) {
    logFile.end ("\nQ" + Date.now() + "\n");
  }
});

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
app.on('ready', createWindow);

function createWindow () {
  // Create the browser window.
  //
  // Kiosk mode doesn't seem right, but adding fullscreen and
  // setMenu(null) seem to achieve the desired effect.  Also adding
  // `alwaysOnTop` may prevent some popups appearing.
  //
  // We set node-integration: false to get more browser-like behaviour for
  // things like jQuery - see the comments mentioning this in both
  //     https://github.com/atom/electron/issues/254 and
  //     https://discuss.atom.io/t/electron-app-to-host-external-site/16390
  // which also give more background on the issue, as does
  //     https://github.com/atom/electron/issues/345
  // Of course this assumes we do not use the features node integration gives
  // us, but presumably, if we did want them, we would have no problem using
  // one of the "require" forms to load jQuery, since we would no longer have
  // a pure web page.
  //
  // The setting of "web-security" is made explicit here, and is false in
  // development as a cheap and cheerful way of allowing ajax call to
  // load a page from CEAP.  This woul be omitted from production code!
  //
  if (typeof (process.env['DEVELOPMENT']) === 'undefined') {
    // Set the zoom state in a global which is read by the preload script
    global.autoZoom = calculateZoom ();
    mainWindow = new BrowserWindow({ kiosk: true,
                                     fullscreen: true,
                                     alwaysOnTop: true,
                                     "webPreferences": {
                                         "zoomFactor": global.autoZoom.zoom,
                                         "nodeIntegration": false,
                                         "webSecurity": true,
                                         "preload": path.join (
                                                        app.getAppPath(),
                                                        "preload.js"
                                                    )
                                     }
                                  });
  } else {
    // Development - use a smaller window and open devtools, and do not
    // run the preload script (whose purpose is to restrict zooming).
    mainWindow = new BrowserWindow({ width:1800, height:1050,
                                     "webPreferences": {
                                         "nodeIntegration": false,
                                         "webSecurity": false
                                     }
                                  });
    mainWindow.openDevTools({mode: "undocked"});
  }
  mainWindow.setMenu (null);

  // and load the index.html
  mainWindow.loadURL ('file://' + __dirname + '/index.html');

  // Emitted when the window is closed.
  // TODO Decide whether we need this at all in our application - we only
  //      have one window, and the application should quit if it is closed.
  mainWindow.on('closed', function() {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null;
  });

  // Start the trivial server for visitor counting
  startHttpServer ();
}

// Production sites will normally have the correct screen size, but we do
// provide a mechanism to allow the zoom to be adjusted automatically to
// cater for strange situations.  A specific installer section has to be
// selected to enable this, via an environment variable, because it can be
// wrong: to use it, the display on which the expo starts must be nominated
// as the "primary" one.
function calculateZoom () {
  var zoom = 1.0;
  if (typeof (process.env['EXPO_AUTO_ZOOM']) !== 'undefined') {
    const {width, height} = electron.screen.getPrimaryDisplay().workAreaSize;
    while (zoom >= 0.19
           && (MIN_SCREEN_WIDTH > width/zoom || MIN_SCREEN_HEIGHT > height/zoom)
          ) {
      zoom -= 0.1;
    }
    return { isAuto: true, zoom: zoom };
  }
  return { isAuto: false, zoom: 1 };
}

// This would be so much nicer in Aff - it might be worth it!
function startHttpServer () {
  try {
    logFile = fs.createWriteStream ("log.txt", {flags: "a"});
    logLastTime = Date.now();
    logFile.write ("\nS" + logLastTime + "\n");
    httpServer = http.createServer (respond);
    httpServer.listen (HTTP_PORT);
  } catch (err) {
    console.log ("Http server failed (" + err.message
                 + ")- continuing without logging");
  }
}

function respond (request, response) {
  try {
    var t = Date.now();
    if (t - logLastTime > 3600000) {
        // More than one hour (in milliseconds) since last timestamp
        logLastTime = t;
        logFile.write ("\nT" + logLastTime + "\n");
    }
    var parts = request.url.split ("."),
        what  = "U";
    if (parts.length > 0) {
        var part = parts.slice(-1)[0];
        if (part.length > 0)
            what = part.charAt(0);
    }
    logFile.write (what);
    if (logLinePos >= 79) {
        logFile.write ("\n");
        logLinePos = 0;
    } else {
        logLinePos += 1;
    }
    response.writeHead (200, { "Content-Length": 2,
                               "Content-Type":   "text/plain"
                             }
                       );
    response.end ("OK", 'utf-8');
  } catch (err) {
    // Try to respond correctly if the server fails at this level
    console.log ("Http transaction failed (" + err.message + ")");
    response.writeHead (500, "Server Error");
    response.end ();
  }
}
