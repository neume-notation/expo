#!/usr/bin/python

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# Convert the JSON data structure to an initialisation of a Purescript
# structure of type ExpoData.All.
#
# The input is JSON decoded from the generated StandAlone/index.html.
# Much of the structure of the target data type is built in here, but not
# the field names of repetitive homogeneous records.  Therefore, attempting
# to compile the resulting Purescript is a valuable check that the JSON
# structure is compatible with the type required for the `expoData` variable.
# An omission in the actual field names provided will result in a type
# error (and has done during development!), and a mismatch in the shape of
# the structure is likely either to produce an error in running this program,
# or to produce a Purescript type error.
#
# This could be used to produce the "mobile" web pages, but the output of
# strings is a little error prone: there could be unhandled escape sequences.
# Therefore, it is run only as a check, generating a second copy of the
# mobile site which is compared against the one produced by a more direct
# use of the JSON.

import sys
import re
import json

def writeInt (data, f, indent):
    f.write (str (int (data)))

def writeNumber (data, f, indent):
    f.write (str (float (data)))

def writeString (data, f, indent):
    f.write ('"'
             + data.replace('"',r'\"').replace("\n",r"\n").encode('utf-8')
             + '"'
            )

# Modify a writer for type `a` to be a writer for type `Nullable a`
#
def nullable (writer):
    def result (data, f, indent):
        if data is None:
            f.write ("toNullable Nothing")
        else:
            f.write ("toNullable (Just (")
            writer (data, f, indent)
            f.write ("))")
    return result

# writeValue is the function to use to write a field value unless the field
# name is mentioned specifically as a key in fieldWriters.
#
def writeRecord (data, f, indent, writeValue, fieldWriters = {}):
    first = True
    f.write ("{")
    for k, v in data.iteritems():
        if indent >= 0:
            f.write ("\n" + indent * " ")
        if first:
            f.write ("  ")
        else:
            f.write (", ")
        f.write (k + ": ")
        if k in fieldWriters:
            writer = fieldWriters[k]
        else:
            writer = writeValue
        writer (v, f, indent + 2)
        first = False
    if indent >= 0:
        f.write ("\n" + indent * " " + "}")
    else:
        f.write ("  }")

def writeArray (data, f, indent, writeValue):
    first = True
    f.write ("[")
    for v in data:
        if indent >= 0:
            f.write ("\n" + indent * " ")
        if first:
            f.write ("  ")
        else:
            f.write (", ")
        writeValue (v, f, indent + 2)
        first = False
    if indent >= 0:
        f.write ("\n" + indent * " " + "]")
    else:
        f.write ("  ]")

def writeMessage (data, f, indent):
    f.write ("Message ")
    writeRecord (data, f, indent, writeString)

def writeLocUrl (data, f, indent):
    f.write ("LocUrl ")
    writeRecord (data, f, indent+2, writeString)

def writeLangUrl (data, f, indent):
    f.write ("LangUrl ")
    writeRecord (data, f, indent+2, writeString)

def writeLocLangUrl (data, f, indent):
    f.write ("LocLangUrl ")
    writeRecord (data, f, indent+2, writeLangUrl)

def writeVideo (data, f, indent):
    writeRecord (data, f, indent+2, writeLangUrl,
                 fieldWriters = {"mp4": nullable (writeLangUrl)} )
    
def writeLocVideo (data, f, indent):
    writeRecord (data, f, indent+2, writeLocLangUrl,
                 fieldWriters = {"mp4": nullable (writeLocLangUrl)} )

def writeTalk (data, f, indent):
    f.write ("Talk ")
    writeRecord (data, f, indent, writeString)

def writeLocTalk (data, f, indent):
    f.write ("LocTalk ")
    writeRecord (data, f, indent+2, writeTalk)

def writeTiming (data, f, indent):
    def writeOneTiming (data, f, indent):
        writeRecord (data, f, indent+2, writeString,
                     fieldWriters = {"name": writeString, "time": writeInt} )
    f.write ("Timing ")
    writeArray (data, f, indent+2, writeOneTiming)

def writeVidTime (data, f, indent):
    f.write ("VidTime ")
    writeRecord (data, f, indent+2, writeTiming)

def writeLocVidTime (data, f, indent):
    f.write ("LocVidTime ")
    writeRecord (data, f, indent+2, writeVidTime)

def writeRectangle (data, f, indent):
    writeRecord (data, f, -1, writeNumber)

def writeActive (data, f, indent):
    f.write ("Active ")
    writeArray (data, f, indent+2, writeRectangle)

def writeLocActive (data, f, indent):
    f.write ("LocActive ")
    writeRecord (data, f, indent+2, writeActive)

def writeHighlight (data, f, indent):
    f.write ("Highlight ")
    writeRecord (data, f, indent+2, writeString,
                 fieldWriters = {"img": writeString, "geom": writeRectangle} )

def writeLocHighlight (data, f, indent):
    f.write ("LocHighlight ")
    writeRecord (data, f, indent+2, nullable (writeHighlight))

def writeNeume (data, f, indent):
    writeRecord (data, f, indent+2, writeString,
                 fieldWriters = {"neume": writeString,
                                 "x":     writeInt,
                                 "y":     writeInt}
                )

def writeChantSyl (data, f, indent):
    def writeArrayNeume (data, f, indent):
        writeArray (data, f, indent+2, writeNeume)
    writeRecord (data, f, indent+2, writeString,
                 fieldWriters = {"ident":  writeInt,
                                 "txt":    writeString,
                                 "neumes": writeArrayNeume}
                )

def writeCeapData (data, f, indent):
    def writeArrayChantSyl (data, f, indent):
        writeArray (data, f, indent+2, writeChantSyl)
    def writeArrayString (data, f, indent):
        writeArray (data, f, -1, writeString)
    def writeArrayArrayString (data, f, indent):
        writeArray (data, f, indent+2, writeArrayString)
    writeRecord (data, f, indent+2, writeString,
                 fieldWriters = {"html":       writeString,
                                 "transcript": writeArrayChantSyl,
                                 "similar":    writeArrayArrayString}
                )

def writeMessages (data, f, indent):
    writeRecord (data, f, indent+2, writeMessage)

def writeUrls (data, f, indent):
    writeRecord (data, f, indent+2, writeString)

def writeLocUrls (data, f, indent):
    writeRecord (data, f, indent+2, writeLocUrl)

def writeLocHLs (data, f, indent):
    writeRecord (data, f, indent+2, writeLocHighlight)

def writeLocActives (data, f, indent):
    writeRecord (data, f, indent+2, writeLocActive)

def writeVideos (data, f, indent):
    writeRecord (data, f, indent+2, writeVideo)

def writeLocVideos (data, f, indent):
    writeRecord (data, f, indent+2, writeLocVideo)

def writeTalks (data, f, indent):
    writeRecord (data, f, indent+2, writeTalk)

def writeLocTalks (data, f, indent):
    writeRecord (data, f, indent+2, writeLocTalk)

def writeTimings (data, f, indent):
    writeRecord (data, f, indent+2, writeVidTime)

def writeLocTimings (data, f, indent):
    writeRecord (data, f, indent+2, writeLocVidTime)

def writePurs (data, f):
    writeRecord ( data, f, 2, writeString,
                  fieldWriters = { "messages":   writeMessages
                                 , "urls":       writeUrls
                                 , "locUrls":    writeLocUrls
                                 , "locHLs":     writeLocHLs
                                 , "locActives": writeLocActives
                                 , "videos":     writeVideos
                                 , "locVideos":  writeLocVideos
                                 , "talks":      writeTalks
                                 , "locTalks":   writeLocTalks
                                 , "timings":    writeTimings
                                 , "locTimings": writeLocTimings
                                 , "miniceap":   writeCeapData
                                 , "feedback":   nullable (writeLangUrl)
                                 }
                )
    f.write ("\n")


# Isolate the JSON value assigned to the expoData variable in the given
# file contents.
def isolate (contents):
    startMark = re.compile (r'^<script>var expoData = ', re.M)
    endMark   =  re.compile (r'^<\/script>$', re.M)
    return re.split (endMark, re.split (startMark, contents)[1]) [0]


def main (argv):
    if len(argv) < 2:
        print >>sys.stderr, "Give the index.html file on the command line!"
        sys.exit(1);
    with open (argv[1], "r") as f:
        contents = f.read()
    try:
        data = json.loads (isolate (contents))
    except ValueError, e:
        raise SystemExit(e)
    writePurs (data, sys.stdout)
    

if __name__ == "__main__":
    main (sys.argv)
