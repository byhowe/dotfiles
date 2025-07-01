module Config.Keybindings
  ( applications
  , multimedia
  , scratchpads
  ) where

import qualified Data.Map as M
import XMonad (XConfig (keys), spawn, KeySym, KeyMask, X)
import XMonad.Util.EZConfig (mkKeymap)
import Data.Map (Map)
import XMonad.Util.NamedScratchpad (namedScratchpadAction, NamedScratchpads)

data BrowserType = Vpn | Clear

browser :: BrowserType -> String
browser Vpn = "firefox-developer-edition -P 'arkenfox-vpn' -new-window"
browser Clear = "mullvad-exclude firefox-developer-edition -P 'arkenfox-clear' -new-window"

newkeys :: XConfig l -> Map (KeyMask, KeySym) (X ()) -> XConfig l
newkeys c keybinds = c { keys = M.union keybinds . keys c }

applications :: XConfig l -> XConfig l
applications c = newkeys c $ mkKeymap c
  [ ("M-a", spawn "alacritty")
  , ("M-w", spawn . browser $ Vpn )
  , ("M-o", spawn . browser $ Clear )
  , ("M-e", spawn "emacs")
  , ("M-d", spawn "pcmanfm")
  , ("M-r", spawn "rofi -show run")
  , ("M-p", spawn "rofi -show drun")
  , ("M-v", spawn "rofi -show window") ]

multimedia :: XConfig l -> XConfig l
multimedia c = newkeys c $ mkKeymap c
  [ ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
  , ("M-<XF86AudioLowerVolume>", spawn "mpc volume -5")
  , ("M-<XF86AudioRaiseVolume>", spawn "mpc volume +5")
  , ("M-<XF86AudioPrev>", spawn "mpc seekthrough -00:00:05")
  , ("M-<XF86AudioNext>", spawn "mpc seekthrough +00:00:05")
  , ("<XF86AudioPrev>", spawn "mpc prev")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("<XF86AudioStop>", spawn "mpc stop")
  , ("M-<Left>",   spawn "mpc seekthrough -00:00:05")
  , ("M-<Right>",  spawn "mpc seekthrough +00:00:05")
  , ("M-<Down>",   spawn "mpc prev")
  , ("M-<Up>",     spawn "mpc next")
  , ("M-C-<Up>",   spawn "mpc toggle")
  , ("M-C-<Down>", spawn "mpc stop")
  , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  ]

scratchpads :: NamedScratchpads -> XConfig l -> XConfig l
scratchpads sps c = newkeys c $ mkKeymap c
  [ ("M-S-a", namedScratchpadAction sps "termleft")
  , ("M-S-z", namedScratchpadAction sps "termright")
  , ("M-S-s", namedScratchpadAction sps "pulsemixer")
  , ("M-S-d", namedScratchpadAction sps "ranger")
  , ("M-S-m", namedScratchpadAction sps "ncmpcpp")
  , ("M-S-p", namedScratchpadAction sps "htop")
  ]
