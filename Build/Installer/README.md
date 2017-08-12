Windows Installer
=================


Quick Start
-----------

The "Haskell build machine" can be the same as the Windows build machine,
but may well be a separate Linux one used most of the development. 

 1. Ensure that ghc and stack are installed on the Haskell build machine,
    and that NSIS version 3 with large strings is installed on the Windows
    build machine (see "System Dependencies" below for more detail).

 2. In this directory, on the Haskell machine, run

        stack build
        stack exec generate-expo-installer

 3. Populate a working directory on Windows as follows:

      - Copy the StandAlone hierarchy, after building CSS, Purescript,
        and the index.html file, keeping the name "StandAlone" under
        the working directory.
        **Do this is a way which dereferences symlinks**  Eg. `scp -r`

      - Unzip the windows electron installation, naming the subdirectory
        "Electron" under the working directory.  We do *not* want the
        additional components (`-symbols` adding extra symbol files, `-pdb`
        adding windows PDB debug files, or `ffmpeg-` replacing ffmpeg with
        a version not containing proprietary codecs), so just download
        and unzip `electron-v1.4.13-win32-x64.zip` from
        https://github.com/electron/electron/releases

 4. Copy the `expo-install.nsi` resulting from step 2 to the working
    directory on Windows.

 5. In the Windows working directory, create the installer by running

        makensis -V3 expo-install.nsi


Features
--------

The installer:

  - Comes in two versions, controlled by the `-s` switch to the Haskell
    program.  If `-s` (--system) is given, the resulting installer expects
    to do a system-wide installation in `\Program Files`, so it requests
    admin permissions.  Otherwise, a user install is done into $APPDATA,
    using ordinary user permissions.  (By the way, Chrome chooses
    $LOCALAPPDATA (...\AppData\Local) instead - I don't know which is
    better.)
    
  - Installs Electron in the under the chosen root.

  - Installs the application in the subdirectory `resources/app` under the
    chosen root (so `resources/app` is the directory which has `package.json`
    at its top level, and the `static` hierarchy immediately below it -- only
    the required files are copied).

  - Creates a small site-dependent javascript file `standalone_params.js`

  - Optionally sets an environment variable EXPO\_AUTO\_ZOOM

  - Creates a start menu shortcut (and desktop shortcut) to set the working
    directory to the full path to APP and run electron.

  - Sets registry entries for install path and uninstaller.

  - We probably do *not* need to add anything to the PATH.


## Building

The installer itself is built in two stages:

  1. The Haskell program in this directory is run to produce an NSIS script,
     using options to customise it with the layout of the build directories.
     There is also an option to control whether the resulting installer puts
     files into system locations, requiring administrator rights, or into 
     the user's own directories.  This can be done on Linux, but see below.

  2. The NSIS script is then compiled with `makensis` to produce the
     actual installer.  Only this part has to be done on Windows.

If the defaults for the path options in `generate-expo-installer` need to
be changed, remember that they refer to the Windows build system where
`makensis` is going to be run.  In this case it may be a little easier
to build and run `generate-expo-installer` on Windows as well.

### System Dependencies

You need NSIS installed (on the Windows machine used to create the installer):

* Download the [NSIS 3.01](http://nsis.sourceforge.net/) installer, run it,
  and place the installation directory on your `PATH`.  Make sure to include
  all components except, perhaps, "Script Examples" and "Start Menu and
  Desktop Shortcuts".
* Patch NSIS with the **large strings build for 3.01** found among its
  [special builds](http://nsis.sourceforge.net/Special_Builds).
  (The patch is applied by extracting the zip archive over top of the NSIS
  directory in "Program Files (x86)".)

An alternative is to use the Unicode NSIS (http://www.scratchpaper.com/),
which already has large strings.


Acknowledgements
----------------

Although deprecated, https://github.com/fpco/minghc/blob/master/README.md
is really useful - some of the notes in the building section come from it.
The GitHub repository at https://github.com/ndmitchell/nsis has documentation
and examples, as well as the source, for the Haskell `nsis` package;
Example2.hs provided many of the hints needed for this installer.
Another good example, which forms the basis of the installer here, is the
installer for `stack`: I think https://github.com/borsboom/stack-installer
is the latest - forked from https://github.com/conklech/stack-installer.
