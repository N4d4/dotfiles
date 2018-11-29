{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
--{-# OPTIONS_GHC -Wno-deprecations #-}
--{-# OPTIONS_GHC -Wno-unused-binds #-}
--{-# OPTIONS_GHC -Wno-unused-imports #-}

------------------------------------------------------------------------}}}
-- MODULES                                                              {{{
---------------------------------------------------------------------------    
import XMonad
import XMonad.Config.Xfce
import System.Exit

-- Custom Module in /lib
--import MyTheme

-- Util
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.CustomKeys           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-CustomKeys.html#v:customKeys ### add submap for sub keybinding

-- Hooks
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

-- Actions
import XMonad.Actions.Promote
--import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CopyWindow            -- like cylons, except x windows
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-Submap.html ## SUBMAPP KEYBIND

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.Gaps               -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Gaps.html
--import XMonad.Layout.Named              -- Deprecated
import XMonad.Layout.Renamed            --in place of Named
import XMonad.Layout.Renamed as R
import XMonad hiding ( (|||) )              -- ||| from X.L.LayoutCombinators
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Hidden             --Test the HideWindow from ethan config
import XMonad.Layout
import XMonad.Layout.ResizableTile      -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-ResizableTile.html
import XMonad.Layout.Circle             -- https://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-Circle.html
import XMonad.Layout.Grid
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SimpleFloat.html#t:SimpleDecoration
import XMonad.Layout.ToggleLayouts      -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-ToggleLayouts.html
import XMonad.Layout.MultiToggle        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-MultiToggle.html
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Minimize           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Minimize.html
import XMonad.Layout.Maximize           -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Maximize.html
import XMonad.Layout.Monitor            -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-Monitor.html
import XMonad.Layout.PositionStoreFloat -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-PositionStoreFloat.html
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.MosaicAlt
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.Accordion
import XMonad.Layout.TwoPane

-- Themes and stuff
import XMonad.Layout.SimpleDecoration   -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-SimpleDecoration.html
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationMadness  -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-DecorationMadness.html
import XMonad.Layout.ImageButtonDecoration
import XMonad.Util.Image
import XMonad.Util.Themes
import XMonad.Util.Loggers              -- for ppExtras: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-Loggers.html

-- Test
-- import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName

import Foreign.C (CChar)
import Data.Char (isSpace, ord)
import Control.Monad (void)
import System.Environment
import System.Posix.IO
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- TODOs
-- try XMonad.Hooks.InsertPosition for new windows behaviors 
-- ComboP for multiple layout plus where to put new window

------------------------------------------------------------------------}}}
-- GLOBALS                                                              {{{
---------------------------------------------------------------------------

-- modMask lets you specify which modkey you want to use.
-- mod1Mask ("left alt")
-- mod3Mask ("right alt") which does not conflict with emacs keybindings.
-- mod4Mask ("windows key")
--
myModMask           = mod4Mask

-- My Workspaces
myWorkspaces        = ["1","2","3","4","5","6","7"]

-- My Default & Alternative Terminal
myTerminal          = "xfce4-terminal"
myAltTerminal       = "kitty"
-- TODO try --remote and --servername to open file in already running session
--myEditor            = "xfce4-terminal -T VIM -e 'vim --servername vim'"
--myEditor            = "xfce4-terminal -T VIM -e vim"
--myEditor            = "xfce4-terminal -e \"/home/nada/.local/bin/spawn_vim.sh\" -T VIM"
--myFileManager       = "xfce4-terminal -e \"/home/nada/.local/bin/spawn_ranger.sh\" -T RANGER"
myEditor            = "kitty -T VIM -e $SHELL -i -c vim"
myFileManager       = "kitty -T RANGER -e $SHELL -i -c ranger"


-- My Launcher
--myLauncher          = "rofi -matching fuzzy -show run"
--myLauncher          = "dmenu_run -i -l 10 -nb \'#1D2426\' -nf \'#8FA388\' -sb \'#8FA388\' -sf \'#1D2426\' -p \'Dmenu:\'"
--myLauncher          = "rofi -show run -fullscreen -padding 280 -sidebar-mode -opacity 85 -separator-style none -bw 0 -hide-scrollbar -color-window \'#002b37\',\'#002b37\',\'#003642\' -color-normal \'#002b37\',\'#819396\',\'#003643\',\'#008ed4\',\'#ffffff\' -color-active \'#002b37\',\'#008ed4\',\'#003643\',\'#008ed4\',\'#66c6ff\' -color-urgent \'#002b37\',\'#da4281\',\'#003643\',\'#008ed4\',\'#890661\' -font \'Fira Code 12\'"
myLauncher          = "rofi -show run -modi window,run,drun -theme ~/.cache/wal/colors-rofi-dark.rasi -fullscreen -padding 280 -sidebar-mode -opacity 85 -separator-style none"


-- Extra Varaibles
myXResolution       = 1920
myYResolution       = 1080
myTopPanelHeight    = 30
myBottomPanelHeight = 30
myDefaultSpacing    = 0
myDefaultGaps       = 0

baseInt = 12
fullInt = baseInt * 2

fullDim = 12 * 2
------------------------------------------------------------------------}}}
-- THEMES                                                               {{{
---------------------------------------------------------------------------
-- Border size
myBorderWidth   = 4

-- Border colors for unfocused and focused windows, respectively.
-- moved in /lib/MyTheme.hs
--myNormalBorderColor  = "#074C34" -- Colors
--myFocusedBorderColor = "#17B753"
myNormalBorderColor  = "#8F7056" -- Colors
myFocusedBorderColor = "#8C5D2D"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
    activeBorderColor   = "#17b753",
    activeTextColor     = "#17b753",
    activeColor         = "#05080C",
    inactiveBorderColor = "#074C34",
    inactiveTextColor   = "#074C34",
    inactiveColor       = "#05080C"
    -- fontName            = "xft:Hurmit Nerd Font:medium:pixelsize:5"
}


--  For GridSelect A green monochrome colorizer based on window class
greenColorizer = colorRangeFromClassName
                  black            -- lowest inactive bg
                  (0x70,0xFF,0x70) -- highest inactive bg
                  black            -- active bg
                  white            -- inactive fg
                  white            -- active fg
    where black = minBound
          white = maxBound

-- | A synthwave monochrome colorizer based on window class
myGoToSelectedColorizer  :: Window -> Bool -> X (String, String)
myGoToSelectedColorizer  = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x78,0x3e,0x57) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

myBringSelectedColorizer :: Window -> Bool -> X (String, String)
myBringSelectedColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg


--myGSFont            = "xft:Noto Sans CJK KR:bold:pixelsize=10"   --"xft:SpoqaHanSansJP:Bold:pixelsize=12"
myGSFont            = "xft:Hurmit NF:medium:size=8"
-- gridSelect select Workspace layout
wsconfig            = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 300
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5
    , gs_originFractY = 0.0
    , gs_font         = myGSFont
    }

-- gridSelect move Workspace layout
wsconfig2           = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 300
    , gs_cellpadding  = 16
    , gs_originFractX = 1.5
    , gs_originFractY = 0.0
    , gs_font         = myGSFont
    }

-- gridSelect select window layout
wiconfig colorizer  = (buildDefaultGSConfig myGoToSelectedColorizer )
--wiconfig = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myGSFont
  }

-- gridSelect bring window layout
wiconfig2 colorizer = (buildDefaultGSConfig myBringSelectedColorizer)
--wiconfig2 = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 16
    , gs_originFractX = 0.5     --1.0
    , gs_originFractY = 0.3     --0.5
    , gs_font         = myGSFont
    }

-- gridSelect popup menu layout
popupconfig         = defaultGSConfig
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myGSFont
    }

-- spawnSelected Redefine
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = popupconfig

------------------------------------------------------------------------}}}
-- SCRATCHPAD                                                           {{{
---------------------------------------------------------------------------  
--http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-NamedScratchpad.html
--Scratchpad Everything:  https://pbrisbin.com/posts/scratchpad_everything/
myScratchPads = [ NS "terminal"       spawnTerm      findTerm      manageTerm       -- and a second
                , NS "youtubeLowLeft" spawnVideoYTL  findVideoYTL  manageVideoYTL   -- and a third
                ]

  where
    role = stringProperty "WM_WINDOW_ROLE"

    spawnTerm       = myTerminal ++ " -T scratchpad"       -- launch my terminal
    findTerm        = title      =? "scratchpad"               -- its window will be named "scratchpad" (see above)
    manageTerm      = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
        where
        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 0.1       -- height, 10% 
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = (1 - w)/2 -- centered left/right

    -- mpsyt : "set playerargs --x11-name mpsyt"
    spawnVideoYTL   = "mpv --x11-name mpsyt --ytdl-format best --no-resume-playback $(xclip -o) || notify-send \"Can't open: $(xclip- o)\""                   -- launch mpv for youtube
    findVideoYTL    = resource  =? "mpsyt" <&&> className =? "mpv"                                                 -- its window will be named "video" (see above)
    manageVideoYTL  = ((customFloating $ W.RationalRect l t w h) <+> doF W.focusDown)                              -- and I'd like it fixed using the geometry below, then focus down, mpv no focus
        where
        h = 0.24                                            -- height, 25%
        w = 0.24                                            -- width,  25%
        t = (1 - h) - (myBottomPanelHeight / myYResolution) -- bottom right edge: bottom xmobar height=16, y resoulution=1080
        l = 1 - w                   -- right, left/right

------------------------------------------------------------------------}}}
-- LAYOUTS                                                              {{{
---------------------------------------------------------------------------    
--http://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-LayoutCombinators.html#g:7
--http://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-LayoutCombinators.html#g:1
-- "Spacing" put a 2px space around every window
-- myLayout = spacing 2 $ Tall 1 (3/100) (1/2)
-- The U & D gaps is > of the Xfce-Panel height
-- TODO try IM layout
-- TODO try Layout.MultiToggle
-- TODO try BSP https://github.com/benweitzman/BinarySpacePartition/
-- TODO XMonad-Layout-LayoutBuilder
-- TODO XMonad-Layout-SubLayouts

-- Transformers (W+f)
mySpacing = spacing baseInt
myGaps = gaps [ (U, baseInt)
              , (D, baseInt)
              , (R, baseInt)
              , (L, baseInt)
              ]

data GAPS = GAPS deriving (Read, Show, Eq, Typeable)
instance Transformer GAPS Window where
    transform GAPS x k = k (avoidStruts $ myGaps $ mySpacing x) (const x)

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform TABBED x k = k myTabA (\_ -> x)

data TEST = TEST deriving (Read, Show, Eq, Typeable)
instance Transformer TEST Window where
    transform TEST x k = k myDefaultLayout (\_ -> x)


named n = renamed [(XMonad.Layout.Renamed.Replace n)] 
myTile  = named "T" $ ResizableTall 1 (3/100) (1/2) []
myMirr  = named "MT" $ Mirror myTile
myMosA  = named "M"  $ MosaicAlt M.empty
myObig  = named "O"  $ OneBig 0.75 0.65
myTabs  = named "TS" $ tabbed shrinkText tabConfig
myTabA  = named "TAS" $ tabbedAlways shrinkText tabConfig
myGrid  = named "G" $ Grid
myCirc  = named "CI" $ Circle
mySpir  = named "SP" $ spiral (6/7)
myFull  = named "FU" $ Full
myAcco  = named "AC" $ Accordion
my3col  = named "3C" $ ThreeColMid 1 (3/100) (1/2)
my2Pan  = named "2C" $ TwoPane 0.03 0.5
myTabM  = named "TM" $ mastered (3/100) (3/5) $ tabbed shrinkText tabConfig
myMaMi  = named "MM" $ Mirror $ mastered (3/100) (4/5) $ Mirror $ myTabs
--myMaMi  = named "MM" $ myTabM

myAllLayouts = myTile ||| myMirr ||| myMosA ||| myObig ||| myTabs ||| myTabA ||| myTabM ||| myGrid ||| myCirc ||| mySpir ||| my3col ||| myFull ||| myAcco ||| my2Pan
myDefaultLayout = myTile ||| myFull
myCodingLayout = myTabM ||| myMaMi
myTestLayouts = myMaMi 

myLayoutHook = avoidStruts
              -- $ gaps [(U,15),(D,15),(L,15),(R,15)] 
              -- $ spacing 10
              -- $ smartSpacingWithEdge 10 
              $ smartBorders
             -- $ lessBorders OnlyFloat
              $ minimize
              $ maximize
              $ mkToggle (single NBFULL)
              $ mkToggle (single TABBED)
              $ mkToggle (single MIRROR)
              $ mkToggle (single REFLECTX)
              $ mkToggle (single REFLECTY)
              $ mkToggle (single GAPS)
              $ mkToggle (single TEST)
              $ onWorkspace (myWorkspaces !! 0) myAllLayouts
              $ onWorkspace (myWorkspaces !! 2) myCodingLayout
              $ onWorkspace (myWorkspaces !! 4) myTabM
              $ onWorkspace (myWorkspaces !! 6) myTestLayouts
              $ myDefaultLayout

------------------------------------------------------------------------}}}
-- KEYBINDINGS                                                          {{{
---------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- need import XMonad.Util.EZConfig
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html   
myKeys1 conf = mkKeymap conf $
    -----------------------------------------------------------------------
    -- System / Utilities Keybindings {{{
    -----------------------------------------------------------------------
    [
    -- Recompile and restart Xmonad
    ("M-o",            spawn "killall compton; killall polybar; xmonad --recompile; xmonad --restart")
    -- Quit Xmonad
    --, ((modm .|. shiftMask, xK_o     ), confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))                    
    ]
    ++

    --------------------------------------------------------------------}}}
    -- Monitor Keybindings {{{
    -----------------------------------------------------------------------
    [
    -- enable both monitor
     ("M-<F12> a", spawn "xrandr --auto --output HDMI-1 --primary --mode 1920x1080 --left-of eDP-1")
    -- disable left monitor
    ,("M-<F12> s", spawn "xrandr --auto --output eDP-1 --off")
    -- disable right monitor
    ,("M-<F12> d", spawn "xrandr --auto --output HDMI-1 --off")
    -- swap screen
    , ("M-<Tab>", swapNextScreen)
    ]
    ++
    --------------------------------------------------------------------}}}
    -- Windows Keybindings {{{
    -----------------------------------------------------------------------
    [
    -- Close focused windows
      ("M-c",          kill)                
    , ("M-S-c",        kill)
    -- Resize view
    , ("M-n",          refresh)
    -- Push floating window back into tiling
    , ("M-t",          withFocused $ windows . W.sink)                               
    -- Toggle focused windows (see toggleFloat bottom )
    , ("M-S-f", withFocused toggleFloat)
    -- focus to the next window                
    --, ("M-<Tab>",      windows W.focusDown)
    , ("M-j",          windows W.focusDown)
    -- focus to previous window               
    , ("M-k",          windows W.focusUp  )
    -- focus to master window               
    , ("M-m",          windows W.focusMaster  )
    -- Go to next workspace
    , ("M-<R>",        nextWS)                             
    , ("M-i",          nextWS)
    -- Go to previous workspace
    , ("M-<L>",        prevWS)
    , ("M-u",          prevWS)
    -- Move the focused window to next workspace
    , ("M-S-<R>",      shiftToNext >> nextWS)              
    , ("M-S-i",        shiftToNext >> nextWS)
    -- Move the focused window to previous workspace
    , ("M-S-<L>",      shiftToPrev >> prevWS)
    , ("M-S-u",        shiftToPrev >> prevWS)
    -- Swap the focused window and the master window            
    , ("M-S-m", windows W.swapMaster)
    -- Swap the focused window with next/previous window               
    , ("M-S-j",        windows W.swapDown  )                
    , ("M-S-k",        windows W.swapUp    )
    ]
    ++

    --------------------------------------------------------------------}}}
    -- Layouts & Sublayouts Keybindings {{{
    -----------------------------------------------------------------------
    [
    -- Rotate through available layouts
    ("M-<Space>", sendMessage NextLayout)
    -- Reset to default layout
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf) 
    -- Orizzontal window resize
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    -- Vertical window resize
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)
    -- Increment/Deincrement the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    -- Toggle struts (avoid bar)
    , ("M-<F11>", sendMessage ToggleStruts)
    -- Toggle Gaps (distance from border)
    , ("M-b 7", sendMessage $ XMonad.Layout.MultiToggle.Toggle GAPS)
    -- Mirror the current layout
    , ("M-b 8 m", sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
    -- Toggle the Full no border layout
    , ("M-f", sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
    , ("M-b 8 t", sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)
    , ("M-b 8 r x", sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
    , ("M-b 8 r y", sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
    , ("M-b 8 q", sendMessage $ XMonad.Layout.MultiToggle.Toggle TEST)
    , ("M-x", setSpacing 10)
    ]
    ++


    --------------------------------------------------------------------}}}
    -- Launcher & App Keybindings {{{
    -----------------------------------------------------------------------
    [
    -- Launch Terminal
    ("M-<Return>", spawn $ XMonad.terminal conf)
    -- Spawn GridSelect
    , ("M-b g", goToSelected defaultGSConfig)
    , ("M-p", spawn myLauncher)
    , ("M-e",   spawn "/home/nada/.local/bin/rofimoji_less.py")
    , ("M-S-e", spawn "/home/nada/.local/bin/rofimoji.py")

    -- runOrRaise == IF(alredy runned){move focus to it} ELSE{run it}
    , ("M-r", runOrRaise "rambox" (className =? "Rambox"))             -- Rambox
    --, ("M-e", runOrRaise "liferea" (className =? "Liferea"))           -- Liferea
    , ("M-d", runOrRaise "thunar" (className =? "Thunar"))             -- Thunar FileManager
    -- runOrRaiseNext == IF(alredy runned OR more than one){move focus to it, or next} ELSE{run it}
    , ("M-q", runOrRaiseNext "pidgin" (className =? "Pidgin"))         -- Pidgin
    , ("M-w", runOrRaiseNext "cherrytree" (className =? "Cherrytree")) -- cherrytree
    , ("M-a", runOrRaiseNext "chromium" (className =? "Chromium"))     -- Chromium
    , ("M-z", runOrRaiseNext "zathura" (className =? "Zathura"))       -- Zathura
    -- raiseNextMaybe == for things that run in terminal
    , ("M-s", raiseNextMaybe (spawn myEditor) (title =? "VIM"))
    , ("M-S-s", spawn myEditor)
    -- Spawn Ranger FileManager
    , ("M-S-d", spawn myFileManager)
    -- Spawn mpv session from URL in CLIPBOARD
    --, ((modm .|. shiftMask, xK_p     ), spawn "mpv \"$(xclip -o)\" --no-hwdec")
    ]
    ++

    --------------------------------------------------------------------}}}
    -- Test Keybindings {{{
    -----------------------------------------------------------------------

    [
    --- Working Window Visible Management
    --, ((modMask x .|. shiftMask, xK_a), windowMenu)
    ("M-b 1", gridselectWorkspace wsconfig  (\ws -> W.view ws))                 -- select workspace
    , ("M-b 2", gridselectWorkspace wsconfig2 (\ws -> W.view ws . W.shift ws))    -- move current window to selected workspace
    --, ((modMask x .|. shiftMask, xK_s), goToSelected defaultGSConfig)           -- show all working apps'name list, if click, then go clicked app. I liked this menu
    --, ((modMask x .|. shiftMask, xK_s), goToSelected wiconfig)                  -- show all working apps'name list, if click, then go clicked app. I liked this menu
    , ("M-b 3", goToSelected  $ wiconfig  myGoToSelectedColorizer)                -- above same, for custom colorizer 
    --, ((modMask x .|. shiftMask, xK_b), bringSelected wiconfig2)                -- bring window list and summon a window you select 
    , ("M-b 4", bringSelected $ wiconfig2 myBringSelectedColorizer)               -- bring window list and summon a window you select 
    --, ("M-b 1", gotoMenu)                                                       -- avobe goToSelected same, like dmenu appearence
    --, ("M-b 1", bringMenu)                                                      -- bring window, like dmenu appaerance
    --, ("M-b 6", sendMessage $ JumpToLayout "tiled")
    --, ("M-b 6", sendMessage $ JumpToLayout "tiled") --vai al layout default
    -- , ("M-b a", withFocused hideWindow) --TOFIX (import layout.hidden
    -- , ("M-b s", popOldestHiddenWindow)
    -- duplicate window to all workspace
    , ("M-b d 1", windows copyToAll)
    , ("M-b d 2", killAllOtherCopies)
    -- minimize / maximize focused windows
    --, ("M-b m", withFocused minimizeWindow)
    --, ("M-b S-m", sendMessage RestoreMinimizedWin)
    ]
    ++

    --------------------------------------------------------------------}}}
    -- Scratchpad Keybindings  {{{
    -----------------------------------------------------------------------
    [
      ("M-b t",   scratchTerm   )
    , ("M-b y l", scratchVideoYTL  )
    ]
     where
           -- this simply means "find the scratchpad in myScratchPads that is 
           -- named terminal and launch it"
           scratchTerm      = namedScratchpadAction myScratchPads "terminal"
           scratchVideoYTL  = namedScratchpadAction myScratchPads "youtubeLowLeft"
           -- When Called If focused windows is Floating then Sink else Float
           toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1 - 0.24) (1 - 0.24 - myBottomPanelHeight / myYResolution) (0.24) (0.24)) s))
                    --else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
    --------------------------------------------------------------------}}}

------------------------------------------------------------------------}}}
-- OLD-KEYBINDINGS                                                      {{{
---------------------------------------------------------------------------   
-- Key bindings. Add, modify or remove key bindings here.
myKeys2 conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
--    -----------------------------------------------------------------------
--    -- System / Utilities Keybindings
--    -----------------------------------------------------------------------
--    [
--    -- Recompile and restart Xmonad
--    ((modm              , xK_o     ), spawn "killall compton; xmonad --recompile; xmonad --restart")
--    -- Quit Xmonad
--    --, ((modm .|. shiftMask, xK_o     ), confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
--    ]
--    ++
--
--
--    -----------------------------------------------------------------------
--    -- Windows Keybindings
--    -----------------------------------------------------------------------
--    [
--    -- Close focused windows
--    ((modm                , xK_c     ), kill)
--    , ((modm .|. shiftMask, xK_c     ), kill)
--    -- Resize view
--    , ((modm,               xK_n     ), refresh)
--    -- Push floating window back into tiling
--    , ((modm,               xK_t     ), withFocused $ windows . W.sink)                 
--    -- focus to the next window                
--    , ((modm,               xK_Tab   ), windows W.focusDown)
--    , ((modm,               xK_j     ), windows W.focusDown)
--    -- focus to previous window               
--    , ((modm,               xK_k     ), windows W.focusUp  )
--    -- focus to master window               
--    , ((modm,               xK_m     ), windows W.focusMaster  )
--    -- Go to next workspace
--    , ((modm,               xK_Right ), nextWS)                             
--    , ((modm,               xK_i     ), nextWS)
--    -- Go to previous workspace
--    , ((modm,               xK_Left  ), prevWS)
--    , ((modm,               xK_u     ), prevWS)
--    -- Move the focused window to next workspace
--    , ((modm .|. shiftMask, xK_Right ), shiftToNext >> nextWS)              
--    , ((modm .|. shiftMask, xK_i     ), shiftToNext >> nextWS)
--    -- Move the focused window to previous workspace
--    , ((modm .|. shiftMask, xK_Left  ), shiftToPrev >> prevWS)
--    , ((modm .|. shiftMask, xK_u     ), shiftToPrev >> prevWS)
--    -- Swap the focused window and the master window            
--    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
--    -- Swap the focused window with next/previous window               
--    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )                
--    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
--    ]
--    ++

    -- mod-[1..7], Switch to workspace N
    -- mod-shift-[1..7], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_7]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-{8,9,0}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{8,9,0}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_8, xK_9, xK_0] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
--    ++

--    -----------------------------------------------------------------------
--    -- Layouts & Sublayouts Keybindings
--    -----------------------------------------------------------------------
--    [
--    -- Rotate through available layouts
--    ((modm,                 xK_space ), sendMessage NextLayout)
--    -- Reset to default layout             
--    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) 
--    -- Shrink the master area
--    , ((modm,               xK_h     ), sendMessage Shrink)
--    -- Expand the master area                 
--    , ((modm,               xK_l     ), sendMessage Expand)                 
--    -- Increment/Deincrement the number of windows in the master area
--    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))       
--    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))                       
--    ]
--    ++
--
--
--    -----------------------------------------------------------------------
--    -- Launcher & App Keybindings
--    -----------------------------------------------------------------------
--    [
--    -- Launch Terminal
--    ((modm,                 xK_Return), spawn $ XMonad.terminal conf)
--    -- Spawn GridSelect
--    , ((modm,               xK_g     ), goToSelected defaultGSConfig)
--    , ((modm .|. shiftMask, xK_p     ), spawn myLauncher)
--
--    -- runOrRaise == IF(alredy runned){move focus to it} ELSE{run it}
--    , ((modm,               xK_r     ), runOrRaise "rambox" (className =? "Rambox"))             -- Rambox
--    , ((modm,               xK_e     ), runOrRaise "liferea" (className =? "Liferea"))           -- Liferea
--    , ((modm,               xK_d     ), runOrRaise "thunar" (className =? "Thunar"))             -- Thunar FileManager
--    -- runOrRaiseNext == IF(alredy runned OR more than one){move focus to it, or next} ELSE{run it}
--    , ((modm,               xK_q     ), runOrRaiseNext "pidgin" (className =? "Pidgin"))         -- Pidgin
--    , ((modm,               xK_w     ), runOrRaiseNext "cherrytree" (className =? "Cherrytree")) -- cherrytree
--    , ((modm,               xK_a     ), runOrRaiseNext "chromium" (className =? "Chromium"))     -- Chromium
--    , ((modm,               xK_s     ), runOrRaiseNext "subl" (className =? "Sublime_text"))     -- Sublime
--    , ((modm,               xK_z     ), runOrRaiseNext "zathura" (className =? "Zathura"))       -- Zathura
--    -- Spawn Ranger FileManager        
--    , ((modm .|. shiftMask, xK_d     ), spawn "xfce4-terminal -e ranger -T ranger")              
--    -- Spawn mpv session from URL in CLIPBOARD
--    --, ((modm .|. shiftMask, xK_p     ), spawn "mpv \"$(xclip -o)\" --no-hwdec")
--    ]
--    ++
--
--
--    -----------------------------------------------------------------------
--    -- TEST & MISC Keybindings
--    -----------------------------------------------------------------------
--    [
--    -- SubMap KeyBinding  mod+b  {n,p,z,space...}
--    ((modm,               xK_b     ), submap . M.fromList $
--        [ ((0, xK_n),     spawn "mpc next")
--        , ((0, xK_p),     spawn "mpc prev")
--        , ((0, xK_z),     spawn "mpc random")
--        , ((0, xK_space), spawn "mpc toggle")
--        , ((0, xK_q), sendMessage Swap)
--        , ((0, xK_w), sendMessage Rotate)
--        , ((0, xK_e), sendMessage $ ExpandTowards L)
--        , ((0, xK_r), sendMessage $ ExpandTowards D)
--        , ((0, xK_t), sendMessage $ ExpandTowards U)
--        , ((0, xK_y), sendMessage $ ExpandTowards R)
--        ])
--    ]

    -----------------------------------------------------------------------
    -- TO TRY
    -----------------------------------------------------------------------

-- Combine ALL the Keybindings
myKeys conf =  (myKeys1 conf) <+> (myKeys2 conf)

------------------------------------------------------------------------}}}
-- WINDOWS RULE                                                         {{{
---------------------------------------------------------------------------  
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- The class name of an application corresponds to the second 
-- value of WM_CLASS (“Pidgin”).
-- The resource corresponds to the first value of WM_CLASS (also “Pidgin”). 
-- The title corresponds to WM_NAME (“Buddy List”).
-- =? EQUAL /=? DIFFERENT <||> OR <&&> AND
myManageHook = composeAll
    [ className =? "Midori"                 --> doShift "2"
    , className =? "Chromium"               --> doShift "2"
    , className =? "Hexchat"                --> doShift "2"
    , className =? "Iceweasel" <||> className =? "Firefox"            --> doShift "2"
    , className =? "Icedove"                --> doShift "2"
    , className =? "Liferea"                --> doShift "2"
    --, resource  =? "mpsyt"                  --> doShift "2"

    -- DEV WORKSPACE
    , className =? "Sublime_text"           --> doShift "3"
    , className =? "Cherrytree"             --> doShift "3"
    , className =? "Zeal"                   --> doShift "3"
    -- , className =? "Gpick"                  --> doShift "3"

    -- MEDIA WORKSPACE
    -- , className =? "vlc"                 --> doFloat <+> doShift "4"
    , className =? "vlc"                    --> doShift "4"
    , className =? "smplayer"               --> doShift "4"
    , className =? "mpv" <&&> resource /=? "mpsyt"                  --> doShift "4"
    , className =? "Evince"                 --> doShift "4"
    , className =? "libprs500"              --> doShift "4"
    , className =? "Vokoscreen"             --> doShift "4"
    , className =? "libreoffice"            --> doShift "4"
    , className =? "libreoffice-calc"       --> doShift "4"
    , className =? "libreoffice-draw"       --> doShift "4"
    , className =? "libreoffice-impress"    --> doShift "4"
    , className =? "libreoffice-math"       --> doShift "4"
    , className =? "libreoffice-writer"     --> doShift "4"

    , className =? "Xmessage"               --> doCenterFloat
    , className =? "Ibus-ui-gtk3"           --> doCenterFloat
    , className =? "Gcr-prompter"           --> doCenterFloat
    , className =? "Pomello"                --> doFloat
    , title     =? "Trasferimento file"     --> doFloat

    -- CHAT WORKSPACE
    -- , className =? "qTox"                   --> doShift "5"
    , className =? "Pidgin"                 --> doShift "5"
    , className =? "Rambox"                 --> doShift "5"

    --FIX
    , isFullscreen                          --> doFullFloat
    , isDialog                              --> doCenterFloat

    --SCRATCHPAD

    ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------}}}
-- STARTUP HOOK                                                         {{{
---------------------------------------------------------------------------  
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X()
myStartupHook = do
    setWMName "LG3D"
    spawn "compton -fCz --backend glx --paint-on-overlay --vsync opengl-swc --glx-no-stencil --glx-no-rebind-pixmap --unredir-if-possible"
    spawn "polybar -r top -c ~/.cache/wal/generated_polybar"
    spawn "polybar -r bottom -c ~/.cache/wal/generated_polybar"
    --     spawn "sleep 3; compton --config '/home/nada/Scaricati/temi/Adapta/wm/openbox-3/compton.conf'"
    --     -- <+> spawn "compton --backend glx -fcC"

------------------------------------------------------------------------}}}
-- LOG HOOK                                                             {{{
---------------------------------------------------------------------------  
-- | Write a string to a property on the root window.  This property is of
-- type UTF8_STRING. The string must have been processed by encodeString
-- (dynamicLogString does this).
xmonadPropLog' :: String -> String -> X ()
xmonadPropLog' prop msg = do
    d <- asks display
    r <- asks theRoot
    xlog <- getAtom prop
    ustring <- getAtom "UTF8_STRING"
    io $ changeProperty8 d r xlog ustring propModeReplace (encodeCChar msg)
 where
    encodeCChar :: String -> [CChar]
    encodeCChar = map (fromIntegral . ord)

-- | Write a string to the _XMONAD_LOG property on the root window.
--xmonadPropLog :: String -> X ()
--xmonadPropLog = xmonadPropLog' "_XMONAD_LOG"

myLogHook :: X ()
myLogHook = do dynamicLogString logPP >>= xmonadPropLog
               return ()
--    where logPP = def 
    where logPP = def {ppLayout = layoutText 
                    }
                    where
                        layoutText "Minimize Maximize T"    = "ReTall"
                        layoutText "Minimize Maximize MT"   = "Mirror"
                        layoutText "Minimize Maximize M"    = "Mosaic"
                        layoutText "Minimize Maximize O"    = "OneBig"
                        layoutText "Minimize Maximize TS"   = "Tabbed"
                        layoutText "Minimize Maximize TAS"  = "TabbedAlways"
                        layoutText "Minimize Maximize TM"   = "Master+Tab"
                        layoutText "Minimize Maximize MM"   = "Master+TabMirror"
                        layoutText "Minimize Maximize G"    = "Grid"
                        layoutText "Minimize Maximize CI"   = "Circle"
                        layoutText "Minimize Maximize SP"   = "Spiral"
                        layoutText "Minimize Maximize FU"   = "Full"
                        layoutText "Minimize Maximize Full" = "Full"
                        layoutText "Minimize Maximize AC"   = "Accordion"
                        layoutText "Minimize Maximize 3C"   = "2column"
                        layoutText "Minimize Maximize 2C"   = "2column"
                        layoutText "Minimize Maximize Spacing T"    = "ReTall"
                        layoutText "Minimize Maximize Spacing MT"   = "Mirror"
                        layoutText "Minimize Maximize Spacing M"    = "Mosaic"
                        layoutText "Minimize Maximize Spacing O"    = "OneBig"
                        layoutText "Minimize Maximize Spacing TS"   = "Tabbed"
                        layoutText "Minimize Maximize Spacing TAS"  = "TabbedAlways"
                        layoutText "Minimize Maximize Spacing TM"   = "Master+Tab"
                        layoutText "Minimize Maximize Spacing MM"   = "Master+TabMirror"
                        layoutText "Minimize Maximize Spacing G"    = "Grid"
                        layoutText "Minimize Maximize Spacing CI"   = "Circle"
                        layoutText "Minimize Maximize Spacing SP"   = "Spiral"
                        layoutText "Minimize Maximize Spacing FU"   = "Full"
                        layoutText "Minimize Maximize Spacing Full" = "Full"
                        layoutText "Minimize Maximize Spacing AC"   = "Accordion"
                        layoutText "Minimize Maximize Spacing 3C"   = "2column"
                        layoutText "Minimize Maximize Spacing 2C"   = "2column"

------------------------------------------------------------------------}}}
-- MAIN CONFIG                                                          {{{
---------------------------------------------------------------------------  
-- Run xmonad with the settings you specify. No need to modify this.
main = xmonad xfceConfig
--main = xmonad def
    { 
     -- simple stuff
        terminal           = myTerminal
        ,borderWidth        = myBorderWidth
        ,modMask            = myModMask
        ,workspaces         = myWorkspaces
        ,normalBorderColor  = myNormalBorderColor
        ,focusedBorderColor = myFocusedBorderColor

     -- key bindings
        ,keys               = myKeys

     -- hooks, layouts (with xfce config)
        ,layoutHook         = myLayoutHook
        ,manageHook         = myManageHook <+> manageHook xfceConfig
     -- fix fullscreen
        ,handleEventHook    = fullscreenEventHook <+> handleEventHook xfceConfig
        ,startupHook        = myStartupHook <+> startupHook xfceConfig
        --,logHook            = myLogHook
        --,logHook = dynamicLogString def >>= xmonadPropLog >> logHook xfceConfig
        ,logHook = myLogHook >> logHook xfceConfig
    }
