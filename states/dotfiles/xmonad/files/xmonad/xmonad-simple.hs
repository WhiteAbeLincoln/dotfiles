import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import Data.Maybe (fromMaybe)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.Navigation2D ( navigation2D, windowGo, windowSwap)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Dmenu
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86
import XMonad.Config (def)
import XMonad.Actions.Commands (defaultCommands, runCommand)
import Data.Monoid (Endo(..))

myWorkspaces :: [String]
myWorkspaces = show <$> [1..9]


--------------------------------------------------------------------------------
-- Colors and Borders
--
myNormalBorderColor :: String
myNormalBorderColor = "#555555"

myFocusedBorderColor :: String
myFocusedBorderColor = "#dab3af"

myBorderWidth :: Dimension
myBorderWidth = 1

-------------------------------------------------------------------------------
-- Key Bindings
--
modm :: KeyMask
modm = mod4Mask -- changes mod key to super

altMask :: KeyMask
altMask = mod1Mask

------------------------------------------------------------------------------
-- Layouts
--

toggleSpacing = setSpacing 0

myLayout =  smartSpacing 10 tall -- ||| renamed [PrependWords "gaps"] $ makeGapped layouts)
    where
        layouts = tall
        -- default tiling algorithm partitions the screen into 2 panes
        tall = Tall nmaster delta ratio

        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100

myConfig = def 
        { modMask            = modm
        , terminal           = "termite"
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , layoutHook         = avoidStruts $ smartBorders myLayout
        , handleEventHook    = fullscreenEventHook <+> handleEventHook def
        } `additionalKeys`
        [
        ((modm, xK_u), toggleSpacing)
        ]

main :: IO()
main = xmonad =<< dzen myConfig
    where
        -- Command to launch the bar.
        myBar = "tee /tmp/xmonad.log"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
        myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
        toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
        -- } `additionalKeys`
        -- [ ((0 , xF86XK_AudioLowerVolume     ),  spawn "ponymix -N decrease 2")
        -- , ((0 , xF86XK_AudioRaiseVolume     ),  spawn "ponymix -N increase 2")
        -- , ((0 , xF86XK_AudioMute            ),  spawn "ponymix -N toggle")
        -- , ((0 , xF86XK_AudioPlay            ),  spawn "playerctl play-pause")
        -- , ((0 , xF86XK_AudioNext            ),  spawn "playerctl next")
        -- , ((0 , xF86XK_AudioPrev            ),  spawn "playerctl previous")
        -- , ((0 , xF86XK_Forward              ),  spawn "playerctl next")
        -- , ((0 , xF86XK_Back                 ),  spawn "playerctl previous")
        -- , ((0 , xF86XK_MonBrightnessUp      ),  spawn "xbacklight -inc 10")
        -- , ((0 , xF86XK_MonBrightnessDown    ),  spawn "xbacklight -dec 10")
        -- , ((modm,                     xK_o  ),  spawn "~/bin/themer")
        -- , ((modm .|. shiftMask,       xK_p  ),  spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
        -- , ((modm,                     xK_p  ),  spawn "rofi -show run")
        -- , ((modm,                   xK_Tab  ),  spawn "rofi -show window")
        -- , ((modm,                     xK_s  ),  spawn "rofi -show ssh")
        -- -- BSP
        -- , ((modm .|. altMask,                 xK_l     ), sendMessage $ ExpandTowards R)
        -- , ((modm .|. altMask,                 xK_h     ), sendMessage $ ExpandTowards L)
        -- , ((modm .|. altMask,                 xK_j     ), sendMessage $ ExpandTowards D)
        -- , ((modm .|. altMask,                 xK_k     ), sendMessage $ ExpandTowards U)
        -- , ((modm .|. altMask .|. controlMask, xK_l     ), sendMessage $ ShrinkFrom R)
        -- , ((modm .|. altMask .|. controlMask, xK_h     ), sendMessage $ ShrinkFrom L)
        -- , ((modm .|. altMask .|. controlMask, xK_j     ), sendMessage $ ShrinkFrom D)
        -- , ((modm .|. altMask .|. controlMask, xK_k     ), sendMessage $ ShrinkFrom U)
        -- , ((modm,                             xK_r     ), sendMessage Rotate)
        -- , ((modm,                             xK_s     ), sendMessage Swap)
        -- , ((modm,                             xK_n     ), sendMessage FocusParent)
        -- , ((modm .|. controlMask,             xK_n     ), sendMessage SelectNode)
        -- , ((modm .|. shiftMask,               xK_n     ), sendMessage MoveNode)
        -- -- NAVIGATION2D
        -- -- , ((mod4Mask .|. shiftMask, xK_space),  switchLayer)
        -- -- Directional navigation of screens
        -- -- , ((mod4Mask,             xK_Right  ),  screenGo R False)
        -- -- , ((mod4Mask,              xK_Left  ),  screenGo L False)
        -- -- , ((mod4Mask,                xK_Up  ),  screenGo U False)
        -- -- , ((mod4Mask,              xK_Down  ),  screenGo D False)
        -- ]
