import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders      (smartBorders)

import           XMonad.Layout.Magnifier
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed         (Theme (..), shrinkText,
                                               tabbedAlways)
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts  (ToggleLayout (..), toggleLayouts)
import           XMonad.Layout.WindowArranger

import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Paste
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Util.Ungrab

import           XMonad.Actions.CycleWS
import           XMonad.Actions.WindowGo

import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ScreenCorners
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP

import qualified XMonad.Util.Hacks            as Hacks

-- EWMH compliance
import           XMonad.Hooks.EwmhDesktops

import qualified Data.Map.Strict              as M
import qualified XMonad.StackSet              as W

-- Magnifier is off by default (can be controlled using arrow keys)
myLayout = smartBorders $
             threeColMid |||
             magnifiercz magnificationFactorH tiled |||
             magnifiercz magnificationFactorV (Mirror tiled) |||
             Full
  where
    magnificationFactorV = 1.384
    magnificationFactorH = 1.621
    tiled   = Tall nmaster delta ratio
    threeCol = ThreeCol nmaster delta ratio
    threeColMid = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 4/100

myStartupHook = do
  addScreenCorners [ (SCUpperRight, nextWS)
                   , (SCUpperLeft, prevWS)
                   ]

myConfig = def
  {
    terminal = "alacritty"

  -- Use Win key instead of Alt
  , modMask = mod4Mask
  , workspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η"]

  -- Styling
  , focusedBorderColor = "#888"
  , normalBorderColor = "#000"
  , borderWidth = 2

  -- Hooks
  , startupHook = myStartupHook
  , layoutHook = screenCornerLayoutHook $ avoidStruts myLayout
  , handleEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook <+> screenCornerEventHook
  } `additionalKeysP` myKeysP
    `removeKeysP` myRemoveKeysP

-- Keybindings to be added/overridden
myKeysP = [
  -- Fit floating windows back to layout
    ("M-S-<Space>", withFocused $ windows . W.sink)

  -- Use rofi to launch programs
  , ("M-p", spawn "rofi -show run")

  -- Map insert key to paste from clipboard
  , ("<Insert>", pasteSelection)

  -- Map print screen to take a screenshot with flameshot
  , ("<Print>", spawn "flameshot gui")

  -- Map audio keys to control volume
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

  -- Map brightness keys to control brightness with brightnessctl
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 20+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 20-")

  -- Map brightness keys + shift to adjust redshift temperature
  , ("S-<XF86MonBrightnessUp>", spawn "echo $(($(cat /tmp/temperature) + 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything")
  , ("S-<XF86MonBrightnessDown>", spawn "echo $(($(cat /tmp/temperature) - 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything")

  -- Reset redshift temperature
  , ("M-S-<XF86MonBrightnessUp>", spawn "echo 3000 > /tmp/temperature && redshift -x")
  , ("M-S-<XF86MonBrightnessDown>", spawn "echo 3000 > /tmp/temperature && redshift -x")

  -- Use power down key to suspend
  , ("<XF86PowerOff>", spawn "systemctl suspend")

  -- FIXME: Spawn firefox in fullscreen, but not in kiosk mode
  , ("M-S-b", spawn "firefox --fullscreen")

  -- Magnify window using arrow keys
  , ("M-<Up>", sendMessage MagnifyMore)
  , ("M-<Down>", sendMessage MagnifyLess)
  ]

-- Keybindings to be removed
myRemoveKeysP = [ "M-t" ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = tertiaryColor " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = brackitify
    , ppHidden          = secondaryColor . wrap " " ""
    , ppHiddenNoWindows = secondaryColor . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppLayout  = \l -> case l of "Tall"                  -> "[]="
                                  "Magnifier Tall"        -> "[]+"
                                  "Magnifier Mirror Tall" -> "+[]"
                                  "Full"                  -> "[ ]"
                                  "ThreeCol"              -> "|||"
                                  _                       -> l
    , ppTitle   = shorten 80
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where

    brackitify = wrap "[" "]"
    formatFocused   = primaryColor . brackitify . ppWindow
    formatUnfocused = secondaryColor . brackitify . ppWindow

    ppWindow = xmobarRaw . (\w -> if null w then "Untitled" else w) . shorten 30

    primaryColor = xmobarColor "#eeeeee" ""
    secondaryColor = xmobarColor "#888888" ""
    tertiaryColor = xmobarColor "#555555" ""
    white    = xmobarColor "#ffffff" ""
    yellow   = xmobarColor "#ff0" ""
    red      = xmobarColor "#ff5555" ""

main :: IO ()
main = do xmonad $ docks $ ewmh $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey myConfig
