import XMonad
import qualified XMonad.StackSet as W
import Data.Maybe (fromMaybe)
import XMonad.Layout.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO()
import XMonad.Config

-----------------------------------------------------------------------------
-- Functions
--
strip ys = filter $ not . (`elem` ys)
-- src <- readFile "/home/abe/.shell-themes/.current_theme"

-----------------------------------------------------------------------------
-- Customized programs
--
-- myScreensaver = "/usr/bin/xautolock -locknow"
myScreensaver = "light-locker-command -l"
toggleScreensaver = "/usr/bin/xautolock -toggle"
myTerminal = "termite -e /usr/bin/tmux"

------------------------------------------------------------------------------
-- Workspaces
--
workspaceMap :: [(String,String)]
workspaceMap = [("term", "\xf120"), ("web", "\xf269"), ("editor", "\xf121"), ("games", "\xf1b6"), ("media", "\xf04b")]

getWorkspace :: String -> String
getWorkspace = fromMaybe "9" . flip lookup workspaceMap

myWorkspaces = foldr (\x acc -> snd x:acc) [] workspaceMap ++ map show [(length workspaceMap + 1)..9]

------------------------------------------------------------------------------
-- Window Rules
-- use xprop | grep WM_CLASS to find the name of a program
--
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myManageHook = composeAll
    [ className =? "Termite"        --> doShift (getWorkspace "term")
    , className =? "Firefox"        --> doShift (getWorkspace "web")
    , className =? "Google-chrome"  --> doShift (getWorkspace "web")
    , className =? "Vivaldi-stable" --> doShift (getWorkspace "web")
    , className =? "GVim"           --> doShift (getWorkspace "editor")
    , className =? "jetbrains-idea" --> doShift (getWorkspace "editor")
    , className =? "Code"           --> doShift (getWorkspace "editor")
    , className =? "Emacs"          --> doShift (getWorkspace "editor")
    , className =? "MultiMC5"       --> doShift (getWorkspace "games")
    , className =? "Steam"          --> doShift (getWorkspace "games")
    , className =? "Spotify"        --> doShift (getWorkspace "media")
    , className =? "Google Play Music Desktop Player" --> doShift (getWorkspace "media")
    , className =? "stalonetray"    --> doFloat
    , isFullscreen                  --> myDoFullFloat
    , manageDocks
    ]

--------------------------------------------------------------------------------
-- Colors and Borders
--
myNormalBorderColor :: String
myNormalBorderColor = "#555555"

myFocusedBorderColor :: String
myFocusedBorderColor = "#dab3af"

myBorderWidth = 1

-------------------------------------------------------------------------------
-- Key Bindings
--
myModMask = mod4Mask -- changes mod key to super

------------------------------------------------------------------------------
-- Layouts
--
myLayout =  tall ||| Mirror tall ||| Full
    where
        -- default tiling algorithm partitions the screen into 2 panes
        tall = Tall nmaster delta ratio

        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100

startup :: X()
startup = do
    spawnOn (getWorkspace "term") myTerminal
    spawnOn (getWorkspace "web") "vivaldi-stable"
    spawnOn (getWorkspace "media") "gpmdp"
    spawn "/home/abe/bin/xmonad-autorun"

main :: IO()
main = do
    -- h <- spawnPipe "nc -l localhost 1234"
    xmonad $ ewmh XMonad.Config.def
        { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , manageHook         = myManageHook
        , layoutHook         = avoidStruts $ smartBorders myLayout
        , startupHook        = startup
        -- , logHook            = dynamicLogWithPP $ XMonad.Hooks.DynamicLog.def { ppOutput = hPutStrLn h }
        -- fix for double tap avoid struts key
        , handleEventHook    = docksEventHook <+> fullscreenEventHook <+> handleEventHook XMonad.Config.def
        } `additionalKeys`
        [ ((0 , xF86XK_AudioLowerVolume    ),  spawn "ponymix -N decrease 2")
        , ((0 , xF86XK_AudioRaiseVolume    ),  spawn "ponymix -N increase 2")
        , ((0 , xF86XK_AudioMute           ),  spawn "ponymix -N toggle")
        , ((0 , xF86XK_AudioPlay           ),  spawn "playerctl play-pause")
        , ((0 , xF86XK_AudioNext           ),  spawn "playerctl next")
        , ((0 , xF86XK_AudioPrev           ),  spawn "playerctl previous")
        , ((0 , xF86XK_Forward             ),  spawn "playerctl next")
        , ((0 , xF86XK_Back                ),  spawn "playerctl previous")
        , ((0 , xF86XK_MonBrightnessUp     ),  spawn "xbacklight -inc 10")
        , ((0 , xF86XK_MonBrightnessDown   ),  spawn "xbacklight -dec 10")
        , ((mod4Mask .|. controlMask, xK_l ), spawn myScreensaver)
        , ((mod4Mask .|. controlMask, xK_c ), spawn toggleScreensaver)
        , ((mod4Mask,                 xK_p ), spawn "rofi -show run")
        , ((mod4Mask,               xK_o   ), spawn "~/bin/themer")
        , ((mod4Mask .|. shiftMask,   xK_p ), spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
        , ((mod4Mask,               xK_Tab ), spawn "rofi -show window")
        , ((mod4Mask,               xK_s   ), spawn "rofi -show ssh")
        , ((mod4Mask, xK_g), goToSelected XMonad.Actions.GridSelect.def)
        ]
