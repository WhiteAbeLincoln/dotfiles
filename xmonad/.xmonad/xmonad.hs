import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

-----------------------------------------------------------------------------
-- Functions
--


-----------------------------------------------------------------------------
-- Customized programs
--
myScreensaver = "/usr/bin/xscreensaver-command --lock"
myBar = "xmobar"
myTerminal = "termite"

------------------------------------------------------------------------------
-- Workspaces
--
myWorkspaces = ["1:term","2:web","3:editor","4:games","5:media"] ++ map show [6..9]

------------------------------------------------------------------------------
-- Window Rules
-- use xprop | grep WM_CLASS to find the name of a program
--
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myManageHook = composeAll
    [ className =? "URxvt"          --> doShift "1:term"
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "google-chrome"  --> doShift "2:web"
    , className =? "Gedit"          --> doShift "3:editor"
    , className =? "MultiMC5"       --> doShift "4:games"
    , className =? "Steam"          --> doShift "4:games"
    , className =? "Spotify"        --> doShift "5:media"
    --, name      =? "Netflix"        --> doShift "5:media"
    , isFullscreen                  --> myDoFullFloat
    , manageDocks
    ]

--------------------------------------------------------------------------------
-- Colors and Borders
--
myNormalBorderColor = "#555555"
myFocusedBorderColor = "#dab3af"

myBorderWidth = 1

-------------------------------------------------------------------------------
-- Key Bindings
--
myModMask = mod4Mask -- changes mod key to super

------------------------------------------------------------------------------
-- Layouts
--
myLayout = named "S_BSP" myBSP ||| emptyBSP ||| named "S_Tall" spaced ||| tall ||| named "Mirror S_Tall" (Mirror spaced) ||| Mirror tall ||| Full
    where
        myBSP = smartSpacing 10 $ emptyBSP

        -- default tiling algorithm partitions the screen into 2 panes
        spaced = smartSpacing 10 $ Tall nmaster delta ratio
        tall = Tall nmaster delta ratio

        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100

ctrlMask = controlMask
altMask = mod1Mask

main = do
    lemonbar <- spawn "barrun"
    xmonad $ ewmh defaultConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , manageHook         = myManageHook
        , layoutHook         = avoidStruts $ smartBorders $ myLayout
        -- fix for double tap avoid struts key
        , handleEventHook    = docksEventHook <+> fullscreenEventHook <+> handleEventHook defaultConfig
        } `additionalKeys`
        [ ((0 , xF86XK_AudioLowerVolume    ),  spawn "ponymix -N decrease 2")
        , ((0 , xF86XK_AudioRaiseVolume    ),  spawn "ponymix -N increase 2")
        , ((0 , xF86XK_AudioMute           ),  spawn "ponymix -N toggle")
        , ((0 , xF86XK_AudioPlay           ),  spawn "mpc toggle")
        , ((0 , xF86XK_Forward             ),  spawn "mpc next")
        , ((0 , xF86XK_Back                ),  spawn "mpc prev")
        , ((mod4Mask .|. controlMask, xK_l ), spawn myScreensaver)
        , ((mod4Mask,                 xK_p ), spawn "rofi -show run")
        , ((mod4Mask .|. shiftMask,   xK_p ), spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
        , ((mod4Mask,               xK_Tab ), spawn "rofi -show window")
        , ((mod4Mask,               xK_s   ), spawn "rofi -show ssh")
        , ((mod4Mask,               xK_o   ), spawn "switcher.sh")
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        ]
