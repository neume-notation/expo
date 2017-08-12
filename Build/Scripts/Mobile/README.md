This code is used only during building.  It creates a set of static pages
suitable for small-screen devices, simply so that users of those devices
can see something useful if they attempt to visit our online version.

Read this code in conjunction with Build/Scripts/makeMobileSite.sh and the
relevant section of Build/Scripts/buildall.sh.  Note particularly that it
depends on two modules Types.purs and ExpoData.js being constructed on the fly
in the src/Base directory.
