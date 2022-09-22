import           System.IO
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BorderResize
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders      (noBorders, smartBorders)
import           XMonad.Layout.ResizableTile  (MirrorResize (..),
                                               ResizableTall (..))
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed         (Theme (..), shrinkText,
                                               tabbedAlways)
import           XMonad.Layout.ToggleLayouts  (ToggleLayout (..), toggleLayouts)
import           XMonad.Layout.WindowArranger
import           XMonad.Util.EZConfig
import           XMonad.Util.Paste
import           XMonad.Util.Run              (spawnPipe)

import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ScreenCorners

import qualified Data.Map.Strict              as M
import qualified XMonad.StackSet              as W

myStartupHook = do
  addScreenCorners [ (SCLowerRight, nextWS)
                   , (SCLowerRight, prevWS)
                   ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def {
        terminal = "alacritty"
      -- Use Win key instead of Alt
      , modMask = mod4Mask
      , workspaces = ["α", "β", "γ", "δ", "ε"]
      -- Styling
      , focusedBorderColor = "#000000"
      , normalBorderColor = "#111111"
      , borderWidth = 2
      -- Hooks
      , startupHook = myStartupHook
      , layoutHook = screenCornerLayoutHook $ layoutHook def
      , handleEventHook = handleEventHook def <+> screenCornerEventHook
      , logHook = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
        }
  } `additionalKeysP`
    -- Keybindings to be added
    [ ("M-S-<Space>", withFocused $ windows . W.sink)
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
    , ("M-q", spawn "xmonad --recompile; pkill xmobar; xmonad --restart")
    ]
    `removeKeysP`
    -- Keybindings to be removed
    [ "M-t" ]

mainManageHook = composeAll
  [ className =? "plank"     --> doIgnore ]
