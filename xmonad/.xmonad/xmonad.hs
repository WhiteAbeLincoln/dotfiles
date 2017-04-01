import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.Navigation2D
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Config

-----------------------------------------------------------------------------
-- Functions
--


-----------------------------------------------------------------------------
-- Customized programs
--
myScreensaver = "/usr/bin/xautolock -locknow"
myBar = "xmobar"
myTerminal = "termite -e /usr/bin/zsh"

------------------------------------------------------------------------------
-- Workspaces
--
myWorkspaces = ["\xf120","\xf269","\xf121","\xf1b6","\xf04b"] ++ map show [6..9]
--             ["1:term","2:web","3:editor","4:games","5:media"]

------------------------------------------------------------------------------
-- Window Rules
-- use xprop | grep WM_CLASS to find the name of a program
--
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myManageHook = composeAll
    [ className =? "Termite"        --> doShift "\xf120"
    , className =? "Firefox"        --> doShift "\xf269"
    , className =? "Google-chrome"  --> doShift "\xf269"
    , className =? "GVim"           --> doShift "\xf121"
    , className =? "jetbrains-idea" --> doShift "\xf121"
    , className =? "MultiMC5"       --> doShift "\xf1b6"
    , className =? "Steam"          --> doShift "\xf1b6"
    , className =? "Spotify"        --> doShift "\xf04b"
    --, name      =? "Netflix"        --> doShift "\xf04b"
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
myLayout =  named "S_Tall" spaced ||| tall ||| named "Mirror S_Tall" (Mirror spaced) ||| Mirror tall ||| Full
    where
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

startup :: X()
startup = do
    spawnOn "\xf120" myTerminal
    spawnOn "\xf269" "google-chrome-stable"
    spawnOn "\xf1b6" "steam"
    spawn "/home/abe/bin/xmonad-autorun"

main = do
    xmonad $ ewmh defaultConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , manageHook         = myManageHook
        , layoutHook         = avoidStruts $ smartBorders $ myLayout
        , startupHook        = startup
        -- fix for double tap avoid struts key
        , handleEventHook    = docksEventHook <+> fullscreenEventHook <+> handleEventHook defaultConfig
        } `additionalKeys`
        [ ((0 , xF86XK_AudioLowerVolume    ),  spawn "ponymix -N decrease 2")
        , ((0 , xF86XK_AudioRaiseVolume    ),  spawn "ponymix -N increase 2")
        , ((0 , xF86XK_AudioMute           ),  spawn "ponymix -N toggle")
        , ((0 , xF86XK_AudioPlay           ),  spawn "playerctl play-pause")
        , ((0 , xF86XK_Forward             ),  spawn "playerctl next")
        , ((0 , xF86XK_Back                ),  spawn "playerctl previous")
        , ((mod4Mask .|. controlMask, xK_l ), spawn myScreensaver)
        , ((mod4Mask,                 xK_p ), spawn "rofi -show run")
        , ((mod4Mask,               xK_o   ), spawn "~/bin/themer")
        , ((mod4Mask .|. shiftMask,   xK_p ), spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
        , ((mod4Mask,               xK_Tab ), spawn "rofi -show window")
        , ((mod4Mask,               xK_s   ), spawn "rofi -show ssh")
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        ]
