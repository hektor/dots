import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP

import           XMonad.Layout.Magnifier
import qualified XMonad.Layout.Magnifier        as Mag (MagnifyMsg (..))
import           XMonad.Layout.NoBorders        (hasBorder, smartBorders)
import           XMonad.Layout.PerScreen
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts    (ToggleLayout (..),
                                                 toggleLayouts)
import           XMonad.Layout.WindowNavigation

import qualified XMonad.StackSet                as W

import           XMonad.Util.EZConfig
import qualified XMonad.Util.Hacks              as Hacks
import           XMonad.Util.Loggers
import           XMonad.Util.Paste
import           XMonad.Util.Run                (spawnExternalProcess,
                                                 spawnPipe)
import           XMonad.Util.Ungrab

-- Statusbar
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = tertiaryColor " | "
    , ppCurrent         = brackitify
    , ppHidden          = secondaryColor
    , ppHiddenNoWindows = tertiaryColor
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppLayout  = \l -> case l of "Tall"                        -> "[]="
                                  "Magnifier Tall"              -> "[]+"
                                  "Magnifier (off) Tall"        -> "[]="
                                  "Magnifier Mirror Tall"       -> "+[]"
                                  "Magnifier (off) Mirror Tall" -> "=[]"
                                  "Full"                        -> "[ ]"
                                  "ThreeCol"                    -> "|||"
                                  _                             -> l
    , ppTitle   = shorten 80
    , ppTitleSanitize   = xmobarStrip
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    brackitify = wrap "[" "]"
    formatFocused   = secondaryColor . brackitify . ppWindow
    formatUnfocused = tertiaryColor . ppWindow

    ppWindow = xmobarRaw . (\w -> if null w then "Untitled" else w) . shorten 16

    primaryColor = xmobarColor "#eeeeee" ""
    secondaryColor = xmobarColor "#aaaaaa" ""
    tertiaryColor = xmobarColor "#888888" ""
    yellow   = xmobarColor "#ff0" ""
    red      = xmobarColor "#ff5555" ""

-- Shift to workspace and view workspace
shiftAndView id = doF (W.view id) <> doF (W.shift id)

-- manageHook
myManageHook = composeAll
                 [
                   className =? "Zathura"    --> doShift "pdf"
                 , className =? "firefox"    --> shiftAndView "www"
                 , className =? "Anki"       --> shiftAndView "etc"
                 , className =? "Obsidian"   --> shiftAndView "etc"
                 , className =? "Launcher"   --> doRectFloat (W.RationalRect 0.05 0.4 0.9 0.5)
                 , className =? "Calculator" --> doCenterFloat
                 , className =? "feh"        --> doCenterFloat
                 , className =? "albert"     --> hasBorder False
                 , className =? "Xournalpp"  --> doRectFloat (W.RationalRect 0.5 0.5 0.5 0.5)
                 ]

-- layoutHook
myLayoutHook= smartBorders $
             -- Column layouts
             threeCol |||
             threeColMid |||
             -- Tiled layouts
             --   Note: magnifier is off by default
             --   (controllable usingarrow keys)
             magnifiercz magnificationFactorH tiled |||
             magnifiercz magnificationFactorV (Mirror tiled) |||
             -- Single window / monocle layout
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
  , manageHook = myManageHook <+> manageHook def
  , layoutHook = avoidStruts myLayoutHook
  , handleEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook
  } `additionalKeysP` myKeysP
    `removeKeysP` myRemoveKeysP

-- Keybindings to be added/overridden
myKeysP = [
  -- Fit floating windows back to layout
    ("M-S-<Space>", withFocused $ windows . W.sink)

  -- Launchers
  , ("M-p", spawn "albert toggle")

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
  , ("M-=", sendMessage MagnifyMore >> sendMessage Mag.ToggleOn)
  , ("M--", sendMessage MagnifyLess >> sendMessage Mag.ToggleOn)
  -- Reset magnification
  , ("M-S-=", sendMessage Mag.ToggleOff)
  -- Layouts
  , ("M-t", sendMessage $ JumpToLayout "Magnifier Tall")
  , ("M-c", sendMessage $ JumpToLayout "ThreeCol")
  , ("M-f", sendMessage $ JumpToLayout "Full")
  ]

-- Keybindings to be removed
myRemoveKeysP = [ ]


main :: IO ()
main = do xmonad $ docks $ ewmh $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey myConfig
