#!/usr/bin/python

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# Process the supplied Julius template (which is supposed to represent valid
# JSON), replacing the entries used for mp4 videos with null.  This is used
# when building the MiniWebServer used to create the standalone installation.

import sys
import re

# This is the pattern for the non-location-dependent file `videos.julius`
#
mp4urls = re.compile (
              r'"mp4":\s+\{(?:[^{}]+\{[^{}]+\})+[^{}]*\}',
              re.DOTALL
          )

# And this is the one for `locVideos.julius`, which has an extra level of
# structure for each entry
#
mp4loc  = re.compile (
              r'"mp4":\s+\{(?:[^{}]+\{(?:[^{}]+\{[^{}]+\})+[^{}]*\})+[^{}]*\}',
              re.DOTALL
          )

def main (argv):
    if len(argv) < 2:
        print >>sys.stderr, "Give a template file on the command line!"
        sys.exit(1);
    with open (argv[1], "r") as f:
        contents = f.read()
    modified = mp4urls.sub ('"mp4": null', contents)
    modified = mp4loc.sub ('"mp4": null', modified)
    sys.stdout.write (modified)

if __name__ == "__main__":
    main (sys.argv)
