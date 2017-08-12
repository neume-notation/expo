{-# LANGUAGE OverloadedStrings #-}
-- Author: Paul Rouse
-- Copyright 2017 University of Bristol
-- Licensed under BSD-3-Clause - see the LICENSE file or http://opensource.org
--
module Main where

import Data.String (IsString(fromString))
import Development.NSIS
import Development.NSIS.Plugins.EnvVarUpdate (setEnvVar, deleteEnvVar)
import Development.NSIS.Plugins.Sections (exactlyOneSection)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)

-- These parameters can be set when generating the NSIS script.  The defaults
-- are OK if the "Quick Start" instructions in README are followed.
data Params = Params {
          systemWide   :: Bool
        , electronPath :: String
        , appPath      :: String
    } deriving Show

defaultParams = Params {
          systemWide   = False
        , electronPath = "Electron"
        , appPath      = "StandAlone"
    }

-- If we ever add a command to modify the user's $PATH, then, to avoid
-- corruption, we *must* use a version of makensis which supports large
-- strings.  Use either Unicode NSIS (http://www.scratchpaper.com/) or the
-- large-strings build from http://nsis.sourceforge.net/Special_Builds.

nsisScript :: Params -> Action ()
nsisScript params = do
    _ <- constantStr "Name" "Expo"

    -- Relative paths (on both source and target).  The target paths are
    -- appended to $INSTDIR *without* a delimiting "/", so they must contain a
    -- leading slash if it is required - this approach allows $ToElectron to
    -- mean the top level of $INSTDIR without using a single-dot component.
    --
    --     - to Electron
    _ <- constantStr "FromElectron" $ fromString $ electronPath params
    _ <- constantStr "ToElectron" ""

    --     - to the top of the application (containing package.json)
    --       Making it `$ToElectron/resources/app` allows electron to find it
    --       without having an explicit parameter (see shortcuts below, too).
    _ <- constantStr "FromApp" $ fromString $ appPath params
    _ <- constantStr "ToApp" "$ToElectron/resources/app"

    -- Slow but good compression, and `Solid` to compress as one large lump
    setCompressor LZMA [Solid]

    name "$Name"

    hKey <- if systemWide params then do
                outFile "expo-install-sys.exe"
                installDir "$PROGRAMFILES64/BristolExpo"
                requestExecutionLevel Admin
                return HKLM
            else do
                outFile "expo-install.exe"
                installDir "$APPDATA/BristolExpo"
                requestExecutionLevel User
                return HKCU
    installDirRegKey hKey "Software/$Name" "Install_Dir"

    ----------------------------------

    -- Pages

    page Directory
    page Components
    page InstFiles

    unpage Components
    unpage InstFiles

    ----------------------------------

    -- The stuff to install
    let mainDesc = Description "Old Hispanic chant manuscript exhibit"
    section "Exhibit (required)" [Required, mainDesc] $ do
        createDirectory "$INSTDIR$ToElectron"
        setOutPath "$INSTDIR$ToElectron"
        file [Recursive] "$FromElectron/*.*"

        createDirectory "$INSTDIR$ToApp"
        setOutPath "$INSTDIR$ToApp"
        file [] "$FromApp/expo.ico"
        file [] "$FromApp/index.html"
        file [] "$FromApp/package.json"
        file [] "$FromApp/preload.js"
        file [] "$FromApp/run.js"
        file [Recursive] "$FromApp/static"

        -- Write the installation path into the registry
        writeRegStr hKey "SOFTWARE/$Name" "Install_Dir" "$INSTDIR"

        -- Write the uninstall keys for Windows, and write the uninstaller
        writeRegStr
            hKey "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
            "DisplayName" "$Name"
        writeRegStr
            hKey "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
            "UninstallString" "\"$INSTDIR/uninstall-expo.exe\""
        writeRegDWORD
            hKey "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
            "NoModify" 1
        writeRegDWORD
            hKey "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
            "NoRepair" 1
        writeUninstaller "uninstall-expo.exe"

    -- Exactly one of these sections must be chosen by the user to determine
    -- the exhibition start page (installed versions, except Valencia, and as
    -- opposed to the web version, are based on a specific manuscript).
    --
    -- Note the double slashes - see the documentation for the `Exp ty` type,
    -- especially the `fromString` function, in the haskell nsis package.
    let homeDesc = Description
                     "Please choose the correct home page for your location. \
                     \If you are not at one of these places, choose Valencia \
                     \since it has the most general home page."
    coimbra <- section "Coimbra home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//start//coimbra';"
                       , "const standAloneIdle = 600;"
                       , "const standAloneLoc  = 'Coimbra';"
                       ]
    lamego <- section "Lamego home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//start//lamego';"
                       , "const standAloneIdle = 600;"
                       , "const standAloneLoc  = 'Lamego';"
                       ]
    leon <- section "Leon home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//start//leon';"
                       , "const standAloneIdle = 600;"
                       , "const standAloneLoc  = 'Leon';"
                       ]
    madrid <- section "Madrid home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//start//madrid';"
                       , "const standAloneIdle = 600;"
                       , "const standAloneLoc  = 'Madrid';"
                       ]
    valencia <- section "Valencia home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//';"
                       , "const standAloneIdle = 600;"
                       , "const standAloneLoc  = 'Valencia';"
                       , "const standAloneLang = 'es';"
                       ]
    bristol <- section "Bristol home page" [homeDesc] $ do
        writeFileLines "$INSTDIR$ToApp/static/expo/js/standalone_params.js"
                       [ "const homeHash       = '//';"
                       , "const standAloneIdle = 600;"
                       ]

    exactlyOneSection [coimbra, lamego, leon, madrid, valencia, bristol]

    -- Optional section (can be disabled by the user)
    let startDesc = Description
                      "Shortcuts will be set in both the start menu and on \
                      \the user's desktop."
    section "Start Menu and Desktop Shortcuts" [startDesc] $ do

        createDirectory "$SMPROGRAMS/$Name"
        setOutPath "$INSTDIR"             -- gets used as the working directory
        createShortcut "$SMPROGRAMS/$Name/Uninstall.lnk"
            [ Target "$INSTDIR/uninstall-expo.exe"
            , IconFile "$INSTDIR/uninstall-expo.exe"
            , IconIndex 0
            , Description "Uninstall $Name"
            ]

        -- If $ToApp is not $ToElectron/releases/app, then electron would need
        -- an explicit location as a parameter, ie we would need to add
        --
        --    , Parameters "."
        --
        -- to the list argument of `createShortcut`
        setOutPath "$INSTDIR$ToApp"      -- gets used as the working directory
        createShortcut "$SMPROGRAMS/$Name/$Name.lnk"
            [ Target "$INSTDIR$ToElectron/electron.exe"
            , IconFile "$INSTDIR$ToApp/expo.ico"
            , IconIndex 0
            , Description "$Name"
            ]

        -- Copy the program-start shortcut to the desktop, too.
        -- This is a little weak in the system-wide case, as we don't seem to
        -- have a way in the Haskell nsis DSL to do setShellContext.
        copyFiles [Silent] "$SMPROGRAMS/$Name/$Name.lnk" "$DESKTOP/$Name.lnk"

    -- Optional section to set the auto-zoom environment variable.
    let zoomDesc = Description
                     "Exhibition sites should not need automatic zoom. Please \
                     \do not choose this setting unless advised to do so \
                     \by Bristol University."
    section "Automatic zoom for small screen" [Unselected, zoomDesc] $ do
        setEnvVar hKey "EXPO_AUTO_ZOOM" "1"

    ----------------------------------

    -- Uninstaller

    uninstall $ do

        -- Delete environment variable (if set)
        deleteEnvVar hKey "EXPO_AUTO_ZOOM"

        -- Remove registry keys
        deleteRegKey hKey "Software/Microsoft/Windows/CurrentVersion/Uninstall/$Name"
        deleteRegKey hKey "SOFTWARE/$Name"

        -- Remove files and uninstaller
        rmdir [Recursive] "$INSTDIR$ToElectron"
        rmdir [Recursive] "$INSTDIR$ToApp"
        delete [] "$INSTDIR/uninstall-expo.exe"

        -- Remove shortcuts, if any
        delete [] "$SMPROGRAMS/$Name/*.*"
        delete [] "$DESKTOP/$Name.lnk"

        -- Remove directories used
        rmdir [] "$SMPROGRAMS/$Name"
        rmdir [] "$INSTDIR"

--------------------------------------

-- Main program

-- Options to set paths in the installer script, at script generation time
opts :: [OptDescr (Params -> Params)]
opts = [ Option "s" ["system"]
                 (NoArg (\params -> params { systemWide = True }))
                 "Default to system-wide install, requiring admin"
       , Option "E" ["electron"]
                 (ReqArg (\e params -> params { electronPath = e }) "DIR")
                 "Directory containing installed Electron"
       , Option "A" ["app"]
                 (ReqArg (\a params -> params { appPath = a }) "DIR")
                 "Directory containing our application"
       ]

main :: IO ()
main = do
    argv <- getArgs
    prog <- getProgName

    params <-
        case getOpt Permute opts argv of
            (o,_,[])   -> return $ foldl (flip id) defaultParams o
            (_,_,errs) -> do
                let header = "\nUsage: " ++ prog ++ " [OPTION...]"
                ioError $ userError $ concat errs ++ usageInfo header opts

    writeFile "expo-install.nsi" $ nsis $ nsisScript params
