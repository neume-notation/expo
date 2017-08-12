This is a completely stand-alone version of the exhibition, in which
almost all files are delivered statically from the filesystem.  They are
specified by relative URLs, but the index page is given as a file:// URL
in run.js, and everything is relative to that.

Run it with

    /path/to/electron-v1.4.13-linux-x64/electron ./

Later versions of [Electron](https://github.com/electron/electron) are
probably fine, but this application has not been tested with them.

**MP4 videos are omitted** from this hierarchy - the way links are made
to the static hierarchy excludes static/videoMp4.

The index.html file is captured from a web version and post-processed to
modify URLs.  This is all done by the script

    Build/Scripts/makeSinglePage.sh

which leaves the built file as `/tmp/index.html`

The resulting file can be checked for URLs which still need attention
by using `egrep http index.html`.  After removing comments and rubbish,
only one should remain:

    "tracker":"http://localhost:3000/expo/count/."  // in urls.julius

This URL is served by a (trivial) server implemented in node - it simply
splits the requested path on "/" and uses the final component to determine
a single character to append to the log.
