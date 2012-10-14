import System.Environment
import System.Exit
 
--import qualified XMonad.StackSet as W
--import qualified Data.Map        as M

import XMonad.Hooks.SetWMName
--import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.DynamicLog

import XMonad.Layout.DwmStyle

--import XMonad.Util.Run

-----------------------------

import XMonad hiding ( (|||) )
import XMonad.Layout hiding ( (|||) )
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.DwmPromote
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
--  import XMonad.Prompt.Shell
import XMonad.Actions.SpawnOn
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.XMonad
import qualified Data.Map as M
import Graphics.X11
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import Data.Ratio ((%))
import System.IO

import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.SimplestFloat
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile

import XMonad.Util.Dzen
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Data.List

import XMonad.Layout.ThreeColumns

main = do
    host <- runProcessWithInput "hostname" [] []
    spawn $ rightBar (init host)
    din <- spawnPipe $ myStatusBar (init host)
    
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      -- simple stuff
        terminal            = myTerminal,
        focusFollowsMouse   = myFocusFollowsMouse,
        workspaces          = myWorkspaces,
        normalBorderColor   = myNormalBorderColor,
        focusedBorderColor  = myFocusedBorderColor,
 
      -- key bindings
        keys                = myKeys,
        mouseBindings       = myMouseBindings,
 
      -- hooks, layouts
        layoutHook          = myLayout,
        manageHook          = myManageHook,
        startupHook         = myStartupHook,
        logHook             = dynamicLogWithPP $ myDzenPP din
    }

myTerminal = "urxvtc"
myWorkspaces = ["sys", "matlab", "web","chat"] ++ map show [5..9]
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#ff0000"
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Statusbar options:
--myTopBar = "conky -c .conkytoprc | dzen2 -x '1000' -y '0' -h '16' -w '600' -ta 'r' -fg '#0f0f0f' -bg '#dfdfdf' -fn '-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1'"

------------------------------------------------------------------------
-- Layouts:
myLayout = windowNavigation $ avoidStruts (smartBorders (dwmStyle shrinkText defaultTheme (tall ||| three ||| im ||| Full ||| simplestFloat)))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 5%100
     three = ThreeColMid 1 (3/100) (1/3)
     im = withIM (1%7) (Title "Buddy List") Grid
     tall = ResizableTall 1 (5/100) (1/2) []
 
------------------------------------------------------------------------
-- Window rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = composeAll . concat $
    [ [placeHook myPlacement ]
    , [ className =? c --> doFloat | c <- myFloatClasses ]
    , [ fmap (isPrefixOf c) title --> doFloat | c <- myFloatTitles ]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ resource  =? "kdesktop"       --> doIgnore ] 
    , [ manageDocks ] 
    , [ isFullscreen --> doFullFloat ] ]
    where 
        myFloatClasses = ["TopLevelShell","com-mathworks-util-PostVMInit","ipython","Ipython","feh","Gimp","bpython","Bpython"]
        myFloatTitles = ["Figure","R Graphics"]
        myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- application management
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)                          -- launch a terminal
    , ((modMask,               xK_space ), shellPromptHere defaultXPConfig { position = Bottom })
    , ((modMask .|. shiftMask, xK_c     ), kill)                                    -- close focused window 
    , ((modMask .|. shiftMask, xK_z     ), spawn "xscreensaver-command -lock")
    -- mpd / audio stuff
    , ((0, xK_F9)     , spawn "mpc toggle")
    , ((0, xK_F10)     , spawn "mpc stop")
    , ((0, xK_F11)     , spawn "amixer set Master 5%-")
    , ((0, xK_F12)     , spawn "amixer set Master 5%+")

    -- layout management
    , ((modMask,                xK_Tab  ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,  xK_n    ), setLayout $ XMonad.layoutHook conf)      -- reset layout
    , ((modMask,                xK_n    ), refresh)                                 -- reset sizes
    , ((modMask,               xK_b    ), sendMessage ToggleStruts)
    , ((modMask .|. controlMask, xK_h    ), sendMessage Shrink)
    , ((modMask .|. controlMask, xK_l    ), sendMessage Expand)
    , ((modMask .|. controlMask, xK_j    ), sendMessage MirrorShrink)
    , ((modMask .|. controlMask, xK_k    ), sendMessage MirrorExpand)
    , ((modMask,                xK_t    ), withFocused $ windows . W.sink)          -- re-tile window
    , ((modMask,                xK_comma), sendMessage (IncMasterN 1))
    , ((modMask,                xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_f     ), sendMessage $ JumpToLayout "Full")

    -- window navigation
    , ((modMask,                xK_j    ), sendMessage $ Go D)
    , ((modMask,                xK_k    ), sendMessage $ Go U)
    , ((modMask,                xK_h    ), sendMessage $ Go L)
    , ((modMask,                xK_l    ), sendMessage $ Go R)
    , ((modMask,                xK_m    ), windows W.focusMaster)
    , ((modMask .|. shiftMask,  xK_j    ), sendMessage $ Swap D)
    , ((modMask .|. shiftMask,  xK_k    ), sendMessage $ Swap U)
    , ((modMask .|. shiftMask,  xK_h    ), sendMessage $ Swap L)
    , ((modMask .|. shiftMask,  xK_l    ), sendMessage $ Swap R)
    , ((modMask .|. shiftMask,  xK_m    ), windows W.swapMaster)
 
    -- Xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "killall conky && killall trayer" >> restart "xmonad" True)
    ]
    ++
 
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] 

    ++
 
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1 or 2,
    -- mod-shift-{w,r}, Move client to screen 1 or 2.
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0,1]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w))
--    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

------------------------------------------------------------------------
-- dynamicLog pretty printer for dzen:

myStatusBar host =
	if host == "ajfrank"
		then "dzen2 -xs 1 -p 1 -x '0' -y '0' -h '" ++ barHeight ++ "' -w '500' -ta 'l' -fg '" ++ wsbarFg ++ "' -bg '" ++ wsbarBg ++ "' -fn '" ++ barFont ++ "'"
    	else "dzen2 -x '0' -y '0' -h '" ++ barHeight ++ "' -w '500' -ta 'l' -fg '" ++ wsbarFg ++ "' -bg '" ++ wsbarBg ++ "' -fn '" ++ barFont ++ "'"
rightBar host =
	if host == "ajfrank"
		then "conky | dzen2 -xs 1 -p 1 -e 'onstart=lower' -h '" ++ barHeight ++ "' -ta 'r' -fg '" ++ wsbarFg ++ "' -bg '" ++ wsbarBg ++ "' -fn '" ++ barFont ++ "'"
		else "conky | dzen2 -e 'onstart=lower' -h '" ++ barHeight ++ "' -ta 'r' -fg '" ++ wsbarFg ++ "' -bg '" ++ wsbarBg ++ "' -fn '" ++ barFont ++ "'"

wsbarFg = "#f0f0ff"
wsbarBg = "#0f0f0f"
barHeight = "16"
barFont = "-xos4-terminus-medium-r-normal-*-12-*-*-*-c-*-iso10646-1"

stripDzen s = aux s [] -- strip dzen formatting to undo ppHidden
  where aux [] acc = acc
        aux x  acc = (\(good,bad) -> aux (dropDzen bad) (acc++good)) $ span (/= '^') x
            where dropDzen b = drop 1 $ dropWhile (/= ')') b

myDzenPP h = defaultPP
    { ppCurrent = wrap (setfg currentBg "[") (setfg currentBg "]") . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppVisible = dzenColor nonEmptyFg nonEmptyBg . wrap " " " " . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHidden =  dzenColor nonEmptyFg nonEmptyBg . wrap " " " " . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = dzenColor emptyFg emptyBg . wrap " " " " . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppUrgent = dzenColor urgentFg urgentBg . wrap " " " " . (\s -> stripDzen s) . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppSep = ""
    , ppWsSep = ""
    , ppTitle = (" : " ++) . dzenColor titleFg titleBg . wrap "< " " >" 
    , ppLayout = dzenColor "#ffffff" "" . layoutToIcon
    , ppOutput = hPutStrLn h
    }
    where
        titleFg = currentFg
        titleBg = currentBg
        currentFg = "#ffffff"
        currentBg = "#0066ff"
        urgentFg = "#ff0000"
        urgentBg = wsbarBg
        nonEmptyFg = "#ffffff"
        nonEmptyBg = wsbarBg
        emptyFg = "#888888"
        emptyBg = wsbarBg

        setfg color stuff = wrap ("^fg(" ++ color ++ ")") "^fg()" stuff
        setbg color stuff = wrap ("^bg(" ++ color ++ ")") "^bg()" stuff

        layoutToIcon x
            | "Mirror Tall" `isInfixOf` x = "mirror ^i(/home/drew/.dzen/icons/layout-mirror-tall.xbm)"
            | "Tall" `isInfixOf` x = "tall ^i(/home/drew/.dzen/icons/layout-tall.xbm)"
            | "Full" `isInfixOf` x = "full ^i(/home/drew/.dzen/icons/layout-full.xbm)"
            | otherwise = x
 
