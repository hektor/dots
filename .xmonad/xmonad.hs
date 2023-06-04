-- {{{
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import qualified XMonad.Layout.Magnifier as Mag (MagnifyMsg (..))
import XMonad.Layout.NoBorders (hasBorder, smartBorders)
import XMonad.Layout.PerScreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
  ( ToggleLayout (..),
    toggleLayouts,
  )
import XMonad.Layout.IndependentScreens
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.Paste
import XMonad.Util.Run
  ( spawnExternalProcess,
    spawnPipe,
  )
import XMonad.Util.Ungrab
-- }}}

-- Statusbar {{{

pp' :: ScreenId -> PP -> PP
pp' s pp = (marshallPP s pp) { ppSort = ppSort pp }

pp :: PP
pp =
  def
    { ppSep = tertiaryColor "  ",
      ppCurrent = brackitify,
      ppHidden = secondaryColor,
      ppHiddenNoWindows = tertiaryColor,
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppLayout = id,
      ppTitle = shorten 80,
      ppTitleSanitize = xmobarStrip,
      ppOrder = \[workspaces, layout, windows, _] -> [workspaces, layout, windows],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    brackitify = wrap "〈" "〉"
    formatFocused = secondaryColor . ppWindow
    formatUnfocused = tertiaryColor . ppWindow

    ppWindow = xmobarRaw . (\w -> if null w then "Untitled" else w) . shorten 16

    primaryColor = xmobarColor "#000000" ""
    secondaryColor = xmobarColor "#333333" ""
    tertiaryColor = xmobarColor "#555555" ""
    yellow = xmobarColor "#ff0" ""
    red = xmobarColor "#ff5555" ""

-- }}}

-- Workspaces & screens {{{

-- Shift to workspace and view workspace
shiftAndView id = doF (W.view id) <> doF (W.shift id)

-- startupHook
myStartupHook =
  do
    spawn "albert"

-- }}}

-- Hooks {{{

-- manageHook
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isDialog --> doCenterFloat,
      className =? "Zathura" --> doShift "1_info",
      className =? "firefox" --> shiftAndView "1_www",
      className =? "firefoxdeveloperedition" --> shiftAndView "1_www",
      className =? "Anki" --> shiftAndView "1_etc",
      className =? "Obsidian" --> shiftAndView "1_etc",
      className =? "Launcher" --> doRectFloat (W.RationalRect 0.05 0.4 0.9 0.5),
      className =? "Zettelkasten" --> doRectFloat (W.RationalRect 0.05 0.4 0.9 0.5),
      className =? "Calculator" --> doCenterFloat,
      className =? "feh" --> doCenterFloat,
      -- Center matplotlib and prevent focus stealing
      -- className =? "matplotlib" --> doRectFloat (W.RationalRect 0.5 0.5 0.5 0.5),
      className =? "matplotlib" --> doCenterFloat,
      className =? "Matplotlib" --> doCenterFloat,
      className =? "Xournalpp" --> doRectFloat (W.RationalRect 0.5 0.5 0.5 0.5),
      className =? "KeePassXC" --> doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8),
      className =? "flameshot" --> doRectFloat (W.RationalRect 0.1 0.1 0.8 0.8)
    ]

-- layoutHook
myLayoutHook =
    showWName' myShowWNameConfig $
    t   |||   -- Tiled layouts
    c3  |||  -- Column layouts
    c3m ||| --
    f       -- Monocle layouts
  where
    t   = renamed [Replace "[]+"] $ centeredIfSingle 0.8 0.9 $ Tall nmaster delta ratio
    c3  = renamed [Replace "|||"] $ ThreeCol nmaster delta ratio
    c3m = renamed [Replace "[|]"] $ ThreeColMid nmaster delta ratio
    f   = renamed [Replace "[+]"] Full
    nmaster = 1
    ratio = 1 / 2
    delta = 4 / 100

myWorkspaces = [ "sh", "www", "dev", "info", "etc" ]
myWorkspaceKeys = [ "a", "s", "d", "f", "g" ]
mySharedWorkspaces = [ "shared" ]
mySharedWorkspaceKeys = [ "1" ]

-- }}}

myConfig =
  def
    { terminal = "alacritty",
      -- Use Win key instead of Alt
      modMask = mod4Mask,
      -- , workspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η"]
      workspaces = withScreen 0 myWorkspaces ++ withScreen 1 mySharedWorkspaces,
      -- Styling
      focusedBorderColor = "#000",
      normalBorderColor = "#0000",
      borderWidth = 4,
      -- Hooks
      startupHook = myStartupHook,
      manageHook = myManageHook <+> manageHook def,
      layoutHook = avoidStruts myLayoutHook,
      handleEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook
    }
    `removeKeysP` myRemoveKeysP
    `additionalKeysP` myKeysP

-- Keybindings to be added/overridden
myKeysP =
  [ -- Fit floating windows back to layout
    ("M-S-<Space>", withFocused $ windows . W.sink),
    -- Launchers
    ("M-S-p", spawn "alacritty --class Launcher,Launcher"),
    ("<F8>", spawn "keepassxc"),
    ("M-p", spawn "albert toggle"),
    -- Map insert key to paste from clipboard
    ("<Insert>", pasteSelection),
    -- Map print screen to take a screenshot with flameshot
    ("<Print>", spawn "flameshot gui"),
    -- Map audio keys to control volume
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    -- Map brightness keys to control brightness with brightnessctl
    ("<XF86MonBrightnessUp>", spawn "brightnessctl set 20+"),
    ("<XF86MonBrightnessDown>", spawn "brightnessctl set 20-"),
    -- Map brightness keys + shift to adjust redshift temperature
    ("S-<XF86MonBrightnessUp>", spawn "echo $(($(cat /tmp/temperature) + 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything"),
    ("S-<XF86MonBrightnessDown>", spawn "echo $(($(cat /tmp/temperature) - 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything"),
    -- Reset redshift temperature
    ("M-S-<XF86MonBrightnessUp>", spawn "echo 3000 > /tmp/temperature && redshift -x"),
    ("M-S-<XF86MonBrightnessDown>", spawn "echo 3000 > /tmp/temperature && redshift -x"),
    -- Use power down key to suspend
    ("<XF86PowerOff>", spawn "systemctl suspend"),
    -- FIXME: Spawn firefox in fullscreen, but not in kiosk mode
    ("M-S-b", spawn "firefox --fullscreen"),
    -- Magnify window using arrow keys
    ("M-=", sendMessage MagnifyMore >> sendMessage Mag.ToggleOn),
    ("M--", sendMessage MagnifyLess >> sendMessage Mag.ToggleOn),
    -- Reset magnification
    ("M-S-=", sendMessage Mag.ToggleOff),
    ("<XF86Calculator>", spawn "alacritty --class 'Calculator' -e ipython -i /home/h/.bin/calc.py"),
    -- playerctl ncspot using arrow keys
    ("M-<Right>", spawn "playerctl next"),
    ("M-<Left>", spawn "playerctl previous"),
    ("M-<Up>", spawn "playerctl play"),
    ("M-<Down>", spawn "playerctl pause")
  ] ++ [
    (m ++ k, windows $ f w) |
      (m, f) <- zip ["M-", "M-S-"]
                    [W.greedyView, W.shift],
                    (k, w) <- zip myWorkspaceKeys (withScreen 0 myWorkspaces) ++ zip mySharedWorkspaceKeys (withScreen 1 mySharedWorkspaces)
  ]

zipKeyPrefixes :: [String] -> [String] -> [String]
zipKeyPrefixes prefixes keys = [prefix ++ key | prefix <- prefixes, key <- keys]

-- Keybindings to be removed
myRemoveKeysP = "M-S-q" : zipKeyPrefixes ["M-", "M-S-"] (map show [ 1..5 ])

-- main :: IO ()
main = do xmonad
  $ ewmh
  $ withEasySB
    (sb1 <> sb2)
    defToggleStrutsKey
    myConfig
  where
    sb1 = statusBarProp "xmobar" $ pure (pp' (S 0) pp)
    sb2 = statusBarProp "xmobar" $ pure (pp' (S 1) pp)
