{-# LANGUAGE ExistentialQuantification #-}
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

-----------------------------------------------------------------------------
-- Functions
--
-- strip :: (Eq a, Foldable t) => t a -> [a] -> [a]
-- strip ys = filter $ not . (`elem` ys)

-----------------------------------------------------------------------------
-- Customized programs
--
-- myScreensaver = "/usr/bin/xautolock -locknow"
myScreensaver :: [Char]
myScreensaver = "light-locker-command -l"
toggleScreensaver :: [Char]
toggleScreensaver = "/usr/bin/xautolock -toggle"
myTerminal :: [Char]
myTerminal = "termite -e /usr/bin/tmux"

------------------------------------------------------------------------------
-- Workspaces
--
workspaceMap :: [(String,String)]
workspaceMap = [("term", "\xf120"), ("web", "\xf269"), ("editor", "\xf121"), ("games", "\xf1b6"), ("media", "\xf04b")]

getWorkspace :: String -> String
getWorkspace = fromMaybe "9" . flip lookup workspaceMap

myWorkspaces :: [String]
myWorkspaces = foldr (\x acc -> snd x:acc) [] workspaceMap ++ map show [(length workspaceMap + 1)..9]

------------------------------------------------------------------------------
-- Window Rules
-- use xprop | grep WM_CLASS to find the name of a program
--
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myManageHook :: Query (Endo WindowSet)
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
jumpLayout = do
  r <- dmenu $ layoutNames ++ gappedNames
  sendMessage $ JumpToLayout r
  where gappedNames = (++ "-gap") <$> layoutNames

layoutNames = ["bsp", "tall", "mirror-tall", "full"]

myLayout =    layouts ||| gapped
-- for some reason, this doesn't work
-- I can still switch to next layout, but jumpLayout doesn't seem to do anything
-- when a gapped layout is selected
-- layouts ||| gapped
-- gapped = renamed [Chain [CutWordsLeft 2, Append "-gap"]] $ smartSpacingWithEdge 10 layouts
    where
        layouts = bsp ||| tall ||| mirrorTall ||| full
        gapped = bspgap ||| tallgap ||| mirrorTallGap ||| fullgap
        bsp  = renamed [Replace "bsp"] emptyBSP
        tall = renamed [Replace "tall"] $ Tall nmaster delta ratio
        mirrorTall = renamed [Replace "mirror-tall"] $ Mirror tall
        full = renamed [Replace "full"] Full
        bspgap = renamed [Replace "bsp-gap"] $ space gap emptyBSP
        tallgap = renamed [Replace "tall-gap"] $ space gap $ Tall nmaster delta ratio
        mirrorTallGap = renamed [Replace "mirror-tall-gap"] $ space gap $ Mirror tall
        fullgap = renamed [Replace "full-gap"] $ space gap Full


        space = smartSpacingWithEdge
        gap = 10
        -- default tiling algorithm partitions the screen into 2 panes
        -- default number of windows in the master pane
        nmaster = 1

        -- default proportion of the screen occupied by master pane
        ratio = 2/3

        -- percent of screen to increment by when resizing panes
        delta = 1/100

makeGapped = undefined

-- switchLayout = do


startup :: X()
startup = do
    spawnOn (getWorkspace "term") myTerminal
    spawnOn (getWorkspace "web") "vivaldi-stable"
    spawnOn (getWorkspace "media") "gpmdp"
    spawnOnce "/home/abe/bin/xmonad-autorun"

main :: IO()
main = do
    xmonad
      $ ewmh
      $ docks
      $ navigation2D def (xK_w, xK_a, xK_s, xK_d) [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)] False
      $ def
        { terminal           = myTerminal
        , modMask            = modm
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , manageHook         = myManageHook
        , layoutHook         = avoidStruts $ smartBorders myLayout
        -- , logHook            = dynamicLogWithPP $ XMonad.Hooks.DynamicLog.def { ppOutput = hPutStrLn h }
        -- , startupHook        = startup
        , handleEventHook    = fullscreenEventHook <+> handleEventHook def
        } `additionalKeys`
        [ ((0 , xF86XK_AudioLowerVolume     ),  spawn "ponymix -N decrease 2")
        , ((0 , xF86XK_AudioRaiseVolume     ),  spawn "ponymix -N increase 2")
        , ((0 , xF86XK_AudioMute            ),  spawn "ponymix -N toggle")
        , ((0 , xF86XK_AudioPlay            ),  spawn "playerctl play-pause")
        , ((0 , xF86XK_AudioNext            ),  spawn "playerctl next")
        , ((0 , xF86XK_AudioPrev            ),  spawn "playerctl previous")
        , ((0 , xF86XK_Forward              ),  spawn "playerctl next")
        , ((0 , xF86XK_Back                 ),  spawn "playerctl previous")
        , ((0 , xF86XK_MonBrightnessUp      ),  spawn "xbacklight -inc 10")
        , ((0 , xF86XK_MonBrightnessDown    ),  spawn "xbacklight -dec 10")
        , ((modm .|. controlMask,     xK_l  ),  spawn myScreensaver)
        , ((modm .|. controlMask,     xK_c  ),  spawn toggleScreensaver)
        , ((modm,                     xK_y  ),  defaultCommands >>= runCommand)
        , ((modm,                     xK_o  ),  spawn "~/bin/themer")
        , ((modm .|. shiftMask,       xK_p  ),  spawn "j4-dmenu-desktop --dmenu='rofi -dmenu'")
        , ((modm,                     xK_p  ),  spawn "rofi -show run")
        , ((modm,                   xK_Tab  ),  spawn "rofi -show window")
        , ((modm,                     xK_s  ),  spawn "rofi -show ssh")
        , ((modm,                     xK_i  ),  jumpLayout)
        -- BSP
        , ((modm .|. altMask,                 xK_l     ), sendMessage $ ExpandTowards R)
        , ((modm .|. altMask,                 xK_h     ), sendMessage $ ExpandTowards L)
        , ((modm .|. altMask,                 xK_j     ), sendMessage $ ExpandTowards D)
        , ((modm .|. altMask,                 xK_k     ), sendMessage $ ExpandTowards U)
        , ((modm .|. altMask .|. controlMask, xK_l     ), sendMessage $ ShrinkFrom R)
        , ((modm .|. altMask .|. controlMask, xK_h     ), sendMessage $ ShrinkFrom L)
        , ((modm .|. altMask .|. controlMask, xK_j     ), sendMessage $ ShrinkFrom D)
        , ((modm .|. altMask .|. controlMask, xK_k     ), sendMessage $ ShrinkFrom U)
        , ((modm,                             xK_r     ), sendMessage Rotate)
        , ((modm,                             xK_s     ), sendMessage Swap)
        , ((modm,                             xK_n     ), sendMessage FocusParent)
        , ((modm .|. controlMask,             xK_n     ), sendMessage SelectNode)
        , ((modm .|. shiftMask,               xK_n     ), sendMessage MoveNode)
        -- NAVIGATION2D
        -- , ((mod4Mask .|. shiftMask, xK_space),  switchLayer)
        -- Directional navigation of screens
        -- , ((mod4Mask,             xK_Right  ),  screenGo R False)
        -- , ((mod4Mask,              xK_Left  ),  screenGo L False)
        -- , ((mod4Mask,                xK_Up  ),  screenGo U False)
        -- , ((mod4Mask,              xK_Down  ),  screenGo D False)
        ]
