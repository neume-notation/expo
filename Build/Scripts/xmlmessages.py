#!/usr/bin/python

# Author: Paul Rouse
# Copyright 2017 University of Bristol
# Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org

# This script produces a JSON version of the short messages, starting from
# the master copy which is in a very specific XML format.
#
# The only reason for using XML is that it provides a simple way of
# communicating with the human beings who are doing the translations into
# other natural languages, using a tool that they are familiar with (Excel).
#
# For this script, only the definitions of the tags and attributes are
# important, but to allow easy communication with a human translator, the
# XML is written in a particular style which is preserved by the combination
# of reading into Excel, writing out from Excel, and then reformatting with
# `xmllint`.
#
# The XML structure is simple and repetitive, and must follow exactly the
# style already used in `templates/data/messages.xml`.  Note also:
#   - Any <message> entry must have <usage> followed by tags for all three
#     languages, and the content of the <english> should always be provided.
#   - When no translation is available initially, the content of the <spanish>
#     and <portuguese> should be set to exactly "TBD", without the quotes,
#     eg <spanish>TBD</spanish>.  This indicates to the human translator that
#     the entry still needs to be done, and it is also treated specially below.
#   - Optionally a temporary <comment> tag may be added at the end of any
#     <message> for one-off communication between humans.  It is removed
#     from the official version of `messages.xml`, as show in step 6 below.
#
# In detail, these steps should be used to transfer to and from Excel:
#
#   1) Open Excel first - don't try to double-click the file
#   2) File/Open messages.xml from Excel, choosing "As an XML table", and OK
#      the message about not having a schema.
#   3) Fix the layout by doing these things on the Home tab:
#        - Select the whole table
#        - Click on "Wrap Text" on the Home tab
#        - Click on the alignment icon for "Top Align"
#        - *Deselect* the whole table, and drag column widths as needed
#        - Select the whole table again, and, on the Home tab, click the
#          "Format" dropdown in the "cells" group - select AutoFit Row Height
#        - Deselect again before storing
#   4) Save as a full xlsx file, and give to translator with instructions:
#        - Leave the "id" column alone!
#        - Do not re-order the rows or columns!
#        - The "usage" column helps you find how and where the item is used
#          when running the program.
#        - Please translate anything marked TBD in the columm for your language
#        - When finished, please save as a full Excel spreadsheet.
#   5) Open the returned Excel file and File / Save As with "Save as type" set
#      to "XML Data" (*not* XML Spreadsheet) - ignore the warning about loss
#      of information.
#   6) Process the resulting .xml file with
#        "xmllint --format FILE | egrep -v '<comment>.*</comment>'"
#   7) Check that the differences from the original occur only where expected!

import xml.etree.ElementTree as ET
import json
import sys

# Wrap certain texts with explicit line breaks.  This only has any effect
# in buttons, which have the CSS "white-space: pre-line;".  Specifying the
# breaks here, as part of the translation to JSON, is the lesser evil, since
# it avoids trying to keep line breaks in any form in the XML while it goes
# back and forth to translators, converted to Excel.  The substition is made
# only when the whole message matches exactly.
wrap = {
    "Hide Video":          "Hide\nVideo",
    "Formato y uso":       "Formato\ny uso",
}

def procMessages (root):
    if root.tag != "messages":
        print >>sys.stderr, "The root element of the XML should be <messages>"
        sys.exit(1)
    result = {}
    for element in root:
        v = procMessage (element)
        result[v["id"]] = v["message"]
    return result

def procMessage (element):
    if element.tag != "message":
        print >>sys.stderr, "First-level XML elements should be <message>"
        sys.exit(1)
    idattr = element.get ("id")
    if idattr is None:
        print >>sys.stderr, "<message> elements should have 'id' attributes"
        sys.exit(1)
    result = { "id": idattr, "message": {} }
    for language in [ "english", "spanish", "portuguese" ]:
        # English is done first to ensure that `tbd` can operate
        elt = element.find (language)
        if elt is None:
            print >>sys.stderr, ( "<message> element " + idattr
                                  + " should have <" + language
                                  + "> sub-element"
                                )
            sys.exit(1)
        lang = shortLang (language)
        result["message"][lang] = tbd (elt.text, result["message"], lang)
    for language in [ "english", "spanish", "portuguese" ]:
        # Do wrapping in a separate loop, so as not to interfere with tbd()
        lang = shortLang (language)
        msg = result["message"][lang]
        result["message"][lang] = wrap.get (msg, msg)
    return result

# `tbd` just recognises the placeholder 'TBD', which is used in the XML as
# a marker for the human translator, and replaces it with a marked version
# of the English (which needs to be done first).
#
def tbd (text, msgSoFar, lang):
    if lang != "en" and text == "TBD":
        return "[" + lang + ": " + msgSoFar["en"] + "]"
    else:
        return text

def shortLang (language):
    return { "english": "en", "spanish": "es", "portuguese": "pt" }[language]

# We want UTF-8 rather than unicode escapes, so set ensure_ascii=False
# The file we produce is not strictly JSON, since it has leading comment
# lines: the comment is guaranteed not to contain '{', so it is easy to
# strip it by dropping characters until the opening '{' is seen.
def generateJson (value):
    result = json.dumps ( value,
                          indent = 4,
                          separators = (',', ': '),
                          sort_keys = True,
                          ensure_ascii = False
                        )
    print "# DO NOT EDIT THIS FILE"
    print "# Generate it from messages.xml using Build/Scripts/xmlmessages.py"
    print "#"
    print result.encode ('utf-8')

def main (argv):
    tree = ET.parse (argv[1])
    root = tree.getroot()
    value = procMessages (root)
    generateJson (value)

if __name__ == "__main__":
    main (sys.argv)
