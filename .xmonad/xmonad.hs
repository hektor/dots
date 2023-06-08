-- {{{

import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.Paste

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

-- }}}

-- Hooks {{{

-- startupHook
myStartupHook =
  do
    spawn "albert"

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

myDynamicManageHook :: ManageHook
myDynamicManageHook =
 composeAll
   [
     title =? "Zettelkasten — Firefox Developer Edition" --> doShift "1_sh"
   ]

-- TODO: Replace showWName by dunst notification
myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = "xft:Iosevka Term SS08:size=16",
      swn_fade = 0.3,
      swn_color = "#111111",
      swn_bgcolor = "#cccccc"
    }

-- layoutHook
myLayoutHook =
    showWName' myShowWNameConfig $
    ifWider smallWidth (
      t   |||   -- Tiled layouts
      c3  |||  -- Column layouts
      c3m ||| --
      f       -- Monocle layouts
    ) (
      t   |||   -- Tiled layouts
      f       -- Monocle layouts
    )
  where
    smallWidth = 1920
    t   = renamed [Replace "[]+"] $ ifWider smallWidth (centeredIfSingle 0.8 0.9 $ Tall nmaster delta ratio)
                                                 (Tall nmaster delta ratio)
    c3  = renamed [Replace "|||"] $ ThreeCol nmaster delta ratio
    c3m = renamed [Replace "[|]"] $ ThreeColMid nmaster delta ratio
    f   = renamed [Replace "[+]"] Full
    nmaster = 1
    ratio = 1 / 2
    delta = 4 / 100

-- }}}

-- Main config {{{

myWorkspaces = [ "sh", "www", "dev", "info", "etc" ]
myWorkspaceKeys = [ "a", "s", "d", "f", "g" ]
mySharedWorkspaces = [ "shared" ]
mySharedWorkspaceKeys = [ "1" ]

myConfig =
  def
    { terminal = "alacritty",
      -- Use Win key instead of Alt
      modMask = mod4Mask,
      workspaces = withScreen 1 myWorkspaces ++ withScreen 2 mySharedWorkspaces,
      -- Styling
      focusedBorderColor = "#000",
      normalBorderColor = "#0000",
      borderWidth = 4,
      -- Hooks
      startupHook = myStartupHook,
      manageHook = myManageHook <+> manageHook def,
      layoutHook = avoidStruts myLayoutHook,
      handleEventHook = dynamicPropertyChange "WM_NAME" myDynamicManageHook <> handleEventHook def <> Hacks.windowedFullscreenFixEventHook
    }
    `removeKeysP` myRemoveKeys
    `additionalKeysP` myKeys
    `additionalMouseBindings` myMouseBindings

-- }}}

-- Keybindings {{{

-- Keybindings to be added/overridden
myKeys :: [(String, X ())]
myKeys =
  [ ("M-<Space> s", unfloatFocusedW),
    ("M-<Space> <Space>", nextLayout),          -- Cycle through layouts
    ("M-<Space> S-<Space>", defaultLayout),     --
    ("M-<Space> M-<Space>", nextLayout),        -- ..fat finger
    ("M-<Space> M-S-<Space>", defaultLayout),   --
    ("M-S-p", spawnTermLauncher),
    ("<F8>",  spawnKeepassXC),
    ("M-z",   spawnZettelkasten),
    ("M-p",   spawnLauncher),
    ("<Insert>", pasteSelection),
    ("<Print>", printScreen),
    ("<XF86AudioRaiseVolume>", raiseVol),       -- Audio volume & playback
    ("<XF86AudioLowerVolume>", lowerVol),       --
    ("<XF86AudioMute>", mute),                  --
    ("M-<Right>", nextTrack),                   --
    ("M-<Left>", prevTrack),                    --
    ("M-<Up>", play),                           --
    ("M-<Down>", pause),                        --
    ("<XF86MonBrightnessUp>", brighten),        -- Brightness & hue controls
    ("<XF86MonBrightnessDown>", dim),           --
    ("S-<XF86MonBrightnessUp>", warm),          --
    ("S-<XF86MonBrightnessDown>", cool),        --
    ("M-S-<XF86MonBrightnessUp>", resetTemp),   --
    ("M-S-<XF86MonBrightnessDown>", resetTemp), --
    ("M-S-b", fullscreenBrowser),
    ("<XF86Calculator>", spawnCalculator),
    ("<XF86PowerOff>", spawn "systemctl suspend") --TODO: Only enable this on laptop
  ] ++
  [ (m ++ k, windows $ f w) |
    (m, f) <- zip ["M-", "M-S-"]
                  [W.greedyView, W.shift],
                  (k, w) <- zip myWorkspaceKeys
                                (withScreen 1 myWorkspaces)
                         ++ zip mySharedWorkspaceKeys
                                (withScreen 2 mySharedWorkspaces)
  ]

zipKeyPrefixes :: [String] -> [String] -> [String]
zipKeyPrefixes prefixes keys = [prefix ++ key | prefix <- prefixes, key <- keys]

-- Keybindings to be removed
myRemoveKeys :: [String]
myRemoveKeys = "M-S-q" : zipKeyPrefixes ["M-", "M-S-"] (map show [ 1..5 ])

myMouseBindings = []

unfloatFocusedW :: X ()
unfloatFocusedW = withFocused $ windows . W.sink

myStartupHook :: X ()
nextLayout = sendMessage NextLayout

defaultLayout :: X ()
defaultLayout = setLayout $ Layout myLayoutHook

spawnZettelkasten :: X ()
spawnZettelkasten = spawn "alacritty --class Zettelkasten,Zettelkasten -e nvim $(cat ~/.zk/current-zettel.txt)"

spawnKeepassXC :: X ()
spawnKeepassXC = spawn "keepassxc"

fullscreenBrowser :: X ()
fullscreenBrowser = spawn "firefox --fullscreen"

spawnLauncher, spawnTermLauncher :: X ()
spawnLauncher = spawn "albert toggle"
spawnTermLauncher = spawn "alacritty --class Launcher,Launcher"
spawnCalculator = spawn "alacritty --class 'Calculator' -e ipython -i /home/h/.bin/calc.py"

printScreen :: X ()
printScreen = spawn "flameshot gui"

raiseVol, lowerVol, mute :: X ()
raiseVol = spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
lowerVol = spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
mute = spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"

nextTrack, prevTrack, play, pause :: X ()
nextTrack = spawn "playerctl next"
prevTrack = spawn "playerctl previous"
play = spawn "playerctl play"
pause = spawn "playerctl pause"

brighten, dim, warm, cool, resetTemp :: X ()
brighten = spawn "brightnessctl set 20+"
dim = spawn "brightnessctl set 20-"
warm = spawn "echo $(($(cat /tmp/temperature) + 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything"
cool = spawn "echo $(($(cat /tmp/temperature) - 50)) > /tmp/temperature && redshift -O $(cat /tmp/temperature) -P && notify < /tmp/temperature -h string:x-canonical-private-synchronous:anything"
resetTemp = spawn "echo 3000 > /tmp/temperature && redshift -x"

-- }}}

-- Main {{{

main :: IO ()
main = do xmonad
  $ ewmh
  $ withEasySB
    (sb1 <> sb2)
    defToggleStrutsKey
    myConfig
  where
    [sb1, sb2] = [statusBarProp "xmobar" $ pure (pp' (S i) pp) | i <- [0..1]]

-- }}}
