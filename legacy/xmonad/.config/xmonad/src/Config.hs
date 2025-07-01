{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Config (config) where

import Data.Default.Class (Default(def))
import Graphics.X11 (xC_left_ptr)
import System.Environment (getProgName)
import System.Exit (exitSuccess)
import XMonad (XConfig (..), Mirror(..), Full(..), mod4Mask, Tall (..), kill, MonadIO (..), restart, className, (=?))
import XMonad.Core (X)
import XMonad.Hooks.DynamicLog ( xmobarStrip, xmobarRaw )
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (withEasySB, statusBarProp, defToggleStrutsKey)
import XMonad.Hooks.StatusBar.PP ( PP(..), shorten, wrap, xmobarBorder, xmobarColor, filterOutWsPP )
import XMonad.Layout ((|||))
import XMonad.Layout.NoBorders (Ambiguity(..), lessBorders)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, customFloating, nsHideOnFocusLoss, scratchpadWorkspaceTag)
import qualified Config.Keybindings as KB
import qualified XMonad.StackSet as W

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "termleft" "alacritty --class termleft" (className =? "termleft") left
  , NS "termright" "alacritty --class termright" (className =? "termright") right
  , NS "pulsemixer" "alacritty --class pulsemixer -e pulsemixer" (className =? "pulsemixer") center
  , NS "ranger" "alacritty --class ranger -e ranger" (className =? "ranger") center
  , NS "ncmpcpp" "alacritty --class ncmpcpp -e ncmpcpp" (className =? "ncmpcpp") thin
  , NS "htop" "alacritty --class htop -e htop" (className =? "htop") center
  ]
  where
    -- from left, from top, width, height
    place p w h = customFloating $ W.RationalRect (p - w / 2) ((1 - h) / 2) w h
    left = place 0.25 0.4 0.9
    right = place 0.75 0.4 0.9
    center = place 0.5 0.6 0.7
    thin = place 0.5 0.4 0.9

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = filterOutWsPP [scratchpadWorkspaceTag] $ def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr


config =
    keybindings
  . fullscreenBorders
  . ewmhFullscreen
  . ewmh
  . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
  $ def
  { modMask = mod4Mask
  , borderWidth = 2
  , focusedBorderColor = "#bd93f9"
  , normalBorderColor = "#12111e"
  , layoutHook = myLayout
  , logHook = refocusLastLogHook >> nsHideOnFocusLoss scratchpads
  , manageHook = namedScratchpadManageHook scratchpads
  , startupHook = myStartupHook
  }
  `additionalKeysP`
  [ ("M-S-q", kill)
  , ("M-S-C-q", liftIO exitSuccess)
  , ("M-S-C-r", liftIO getProgName >>= (`restart` True))
  ]
  where
    keybindings = KB.applications . KB.scratchpads scratchpads . KB.multimedia
    fullscreenBorders c = c { layoutHook = lessBorders OnlyScreenFloat $ layoutHook c }
