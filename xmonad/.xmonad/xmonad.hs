import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Actions.GridSelect
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
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
myTerminal = "urxvt"

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
    , className =? "Steam"          --> doFloat <+> doShift "4:games"
    --, name      =? "Netflix"        --> doShift "5:media"
    , isFullscreen                  --> myDoFullFloat
    , manageDocks
    ]

--------------------------------------------------------------------------------
-- Colors and Borders
--
myNormalBorderColor = "#555555"
myFocusedBorderColor = "#dab3af"

xmobarTitleColor = "#dab3af"
xmobarCurrentWorkspaceColor = "#aec29b"
myBorderWidth = 1

-------------------------------------------------------------------------------
-- Key Bindings
--
myModMask = mod4Mask -- changes mod key to super

------------------------------------------------------------------------------
-- Layouts
-- 
myLayout = named "BSP" myBSP ||| named "Tall" spaced ||| named "Mirror Tall" (Mirror spaced) ||| Full 
    where
        
        myBSP = smartSpacing 10 $ emptyBSP
        
        -- default tiling algorithm partitions the screen into 2 panes
        spaced = smartSpacing 10 $ Tall nmaster delta ratio
        
        -- default number of windows in the master pane
        nmaster = 1
        
        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100


baseConfig = defaultConfig

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myPP = xmobarPP { ppTitle = xmobarColor xmobarTitleColor "" . shorten 100 -- sets the title color and shortens at 100 chars
                , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                , ppVisible = wrap "{" "}"
                }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = baseConfig 
    { terminal           = myTerminal
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth        = myBorderWidth
    , manageHook         = myManageHook
    , layoutHook         = avoidStruts $ smartBorders $ myLayout 
    -- , startupHook        = setWMName "LG3D"
    -- fix for double tap avoid struts key
    , handleEventHook    = docksEventHook <+> handleEventHook defaultConfig
    } `additionalKeys`
    [ ((0 , xF86XK_AudioLowerVolume    ),  spawn "pactl set-sink-volume '@DEFAULT_SINK@' -2% && /home/abe/bin/volume.sh")
    , ((0 , xF86XK_AudioRaiseVolume    ),  spawn "pactl set-sink-volume '@DEFAULT_SINK@' +2% && /home/abe/bin/volume.sh")
    , ((0 , xF86XK_AudioMute           ),  spawn "pactl set-sink-mute '@DEFAULT_SINK@' toggle && /home/abe/bin/volume.sh")
    , ((mod4Mask .|. controlMask, xK_l ), spawn myScreensaver)
    , ((mod4Mask .|. shiftMask, xK_p   ), spawn "j4-dmenu-desktop")
    , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
    ]
