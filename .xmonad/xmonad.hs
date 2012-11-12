--
-- xmonad example config file for xmonad-0.10
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- Last edited on 2012-08-07
--

import XMonad
import XMonad.Actions.WindowGo
import XMonad.Actions.KeyRemap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

-- Handle fullscreen events ---------------------
-- Apps such as Chromium emit correct ewmh events and are handled
-- properly while apps such as VLC use other fullscreen event messages
-- and require X.L.Fullscreen, so we import both and reference
-- X.H.EmwhDesktops as E
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageHelpers

-------------------------------------------------

import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane

import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe, hPutStrLn)

import Data.Monoid
import Data.List
import Data.Maybe
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#161616"
myFocusedBorderColor = "#0f0f0f"

------------------------------------------------------------------------
-- Custom "functions" and other misc. variables get set here
_restartXMonad = "killall conky dzen2 trayer xscreensaver; xmonad --recompile; xmonad --restart"
_spawnScreenSaver = "xscreensaver -nosplash"
_lockScreen = "xscreensaver-command -lock"

_dmenuOptions = "-i -fn '-*-terminus-bold-*-*-*-14-*-*-*-*-*-iso10646-*'`"
_dmenu = "`dmenu_run " ++ _dmenuOptions
_customDmenu = "exe=`cat ~/.xmonad/custom_path_list | dmenu " ++ _dmenuOptions ++ " && PATH=$PATH:~/bin && eval \"exec $exe\""

_gmrun = "gmrun"
_takeScreenShot = "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/images/screenshots/'"
_spaceNames = [("1","1:terminal"), ("2", "2:web"), ("3", "3:irc"), ("4", "4:misc"), ("5", "5:misc"), ("6", "6:misc"), ("7", "7:misc"), ("8", "8:misc"), ("9", "9:misc")]

_statusBarHeight = "24"
_statusBarFontSize = "14"
_statusBarBorderCoord = "1294" --originally '1310'
_statusBarBgColor = "1c1c1c"
_statusBarLeft = "conky -c ~/.xmonad/conky_statusrc_left | dzen2 -x '0' -y '0' -h '" ++ _statusBarHeight ++ "' -w '" ++ _statusBarBorderCoord ++ "' -ta 'l' -fg '#808080' -bg '#" ++ _statusBarBgColor ++ "' -fn '-*-terminus-bold-*-*-*-" ++ _statusBarFontSize ++ "-*-*-*-*-*-iso10646-*'"
_statusBarRight = "conky -c ~/.xmonad/conky_statusrc_right | dzen2 -x '" ++ _statusBarBorderCoord ++ "' -y '0' -h '" ++ _statusBarHeight ++ "' -w '290' -ta 'r' -fg '#808080' -bg '#" ++ _statusBarBgColor ++ "' -fn '-*-terminus-bold-*-*-*-" ++ _statusBarFontSize ++ "-*-*-*-*-*-iso10646-*'"
_systemTray = "trayer --edge top --align right --SetPartialStrut true --height " ++ _statusBarHeight ++ " --widthtype request --tint 0x" ++ _statusBarBgColor ++ " --transparent true --alpha 0"

-- Configuration of the "pretty printer" (PP) for dzen2
_statusPP h screen = defaultPP
   {  ppCurrent         = wrapFgBg "#66cc66" "#333333" . \wsID -> dropIx wsID
   ,  ppVisible         = wrapFgBg "#3399ff" "#333333" . \wsID -> dropIx wsID
   ,  ppHidden          = wrapFg "#eeeeee"             . \wsID -> dropIx wsID
   ,  ppHiddenNoWindows = wrapFg "#999999"             . \wsID -> dropIx wsID
   ,  ppUrgent          = wrapBg "#00ff00"
   ,  ppSep    = " ^fg(#999999)^r(1x14) "
   ,  ppWsSep  = " "
   ,  ppTitle  = (\x -> " " ++ wrapFg "#66cc66" x)
   ,  ppOrder  = if ([screen] == ['1']) then \(ws:l:_:_) -> [ws,l] else id
   ,  ppLayout = dzenColor ("" ++ "#eeeeee" ++ "") "" .
               (\x -> case x of
                  "Hinted Spacing 4 ResizableTall"        -> "RT"
                  "Hinted Mirror Spacing 4 ResizableTall" -> "MRT"
                  "Hinted Full"                           -> "F"
                  "Hinted Spacing 4 TwoPane"              -> "2P"
                  otherwise -> x
               )
   ,  ppOutput = hPutStrLn h
   }
   where
      wrapFgBg fgColor bgColor content = wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
      wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
      wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
--      dropIx wsId = if (isPrefixOf [screen] wsId) then drop 2 wsId else ""
      dropIx wsId = if (isPrefixOf [screen] wsId) then fromMaybe "" (lookup wsId _spaceNames) else ""

-- Configure the event pop-up notifications using dzen2
_urgency = withUrgencyHookC dzenUrgencyHook {
      args = [
         "-x", "0", "-y", "0", "-h", _statusBarHeight, "-w", "1600",
         "-ta", "l",
         "-fn", "'-*-terminus-bold-*-*-*-12-*-*-*-*-*-iso10646-*'",
         "-bg", "#af8700",
         "-fg", "#1c1c1c"
         ] } urgencyConfig {
      suppressWhen = Focused,
      remindWhen = Every (minutes 3)
      }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu with a customized program list
    , ((modm,               xK_p     ), spawn _customDmenu)

    -- launch dmenu
    , ((modm .|. shiftMask, xK_p     ), spawn _dmenu)

    -- launch gmrun
    , ((modm .|. shiftMask, xK_o     ), spawn _gmrun)

    -- lock the screen
    , ((modm .|. shiftMask, xK_z     ), spawn _lockScreen)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Shrink second dimension of area
    , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)

    -- Expand second dimension of area
    , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn _restartXMonad)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- 'avoidStruts' allows for a properly working status bar
-- 'smartBorders' removes borders when only one window is viewable or an
--     application went into fullscreen mode
--
--Modification by Vic Fryzel
--myLayout = avoidStruts $ (resizable ||| Mirror resizable ||| Full ||| noBorders (fullscreenFull Full))
myLayout = avoidStruts $ smartBorders $ layoutHints $ (resizable ||| Mirror resizable ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled     = spacing space $ Tall nmaster delta ratio

     -- custom tiling algorithm with resizable parameters
     resizable = spacing space $ ResizableTall nmaster delta ratio []

     -- some two pane thing. I do not know what this means.
     twopane   = spacing space $ TwoPane delta ratio

     -- The default number of windows in the master pane
     nmaster   = 1

     -- Default proportion of screen occupied by master pane
     ratio     = 1/2

     -- Percent of screen to increment by when resizing panes
     delta     = 3/100

     -- Space between windows (default = 2)
     space     = 0

------------------------------------------------------------------------
-- Window rules:

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
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? c            --> doFloat | c <- myClassNameFloats ]
    , [(className =? "Firefox"   <&&> resource =? "Dialog")  --> doFloat ]
    , [(className =? "Firefox"   <&&> title =? c) --> doFloat | c <- myFirefoxTitleFloats ]
    , [(className =? "Hexchat"   <&&> title =? c) --> doFloat | c <- myHexchatTitleFloats ]
    , [(className =? "Chromium"  <&&> title =? c) --> doFloat | c <- myChromiumTitleFloats ]
    , [(className =? "Gimp"      <&&> title =? c) --> doFloat | c <- myGimpTitleFloats ]
    , [(className =? "Wireshark" <&&> title =? c) --> doFloat | c <- myWiresharkTitleFloats ]
    , [(className =? "Dropbox"   <&&> title =? c) --> doFloat | c <- myDropboxTitleFloats ]
    , [ className =? "Chromium"   --> doShift "2" ]
    , [ title =? "HexChat"        --> doShift "3" ]
    , [ resource  =? r            --> doIgnore | r <- myIgnores ]
-- Fullscreen modification by Vic Fryzel
--    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]
    ]
  where myClassNameFloats = [ "MPlayer", "xmessage", "XClock" ]
        myFirefoxTitleFloats = [ "Downloads", "Library", "Error Console", "Firefox Sync Setup", "Firefox Preferences", "Clear Private Data", "About Mozilla Firefox", "Session Manager - Mozilla Firefox", "Adblock Plus Filter Preferences" ]
        myHexchatTitleFloats = [ "Select font", "Select an Image File", "Select Download Folder", "Select an output filename", "HexChat: Plugins and Scripts", "Select a Plugin or Script to load", "Enter Channel to Join:" ]
        myChromiumTitleFloats = [ "Learning System Browser Checker - Chromium", "Compose Mail - stjohn.jason@gmail.com - Gmail - Chromium", "Save File" ]
        myGimpTitleFloats = [ "Tool Options", "Preferences" ]
        myWiresharkTitleFloats = [ "Wireshark" ]
        myDropboxTitleFloats = [ "Select a Folder" ]
        myIgnores = [ "desktop_window", "kdesktop", "trayer" ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- The default: `myEventHook = mempty`
-- TODO: Decide whether E.ewmhDesktopsEventHook should be used as well.
myEventHook = E.fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- The default: `myLogHook = return ()`
-- TODO: Look into `logHook = dynamicLogWithPP dzenPP`
-- TODO: Reference http://osdir.com/ml/xmonad@haskell.org/2009-09/msg00186.html
myLogHook = E.ewmhDesktopsLogHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
   ws     <- spawnPipe _statusBarLeft
   ws     <- spawnPipe _statusBarRight
   ws     <- spawnPipe _systemTray
   ws     <- spawnPipe _spawnScreenSaver

   xmonad $ _urgency $ E.ewmh defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
