module XMonad.Custom.Manage.ManageHook
  ( manageHook
  )
where

import Data.List
import XMonad hiding (manageHook)
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers hiding ((~?))
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import XMonad.Custom.Manage.ManageHelpers
import XMonad.Custom.Scratchpads

(=^) :: (Eq a, Functor f) => f [a] -> [a] -> f Bool
q =^ x = fmap (x `isPrefixOf`) q

composeActions :: [MaybeManageHook]
composeActions =
  [ appName =? "emacs-popup" -?> tileBelowNoFocus
  , appName =? "emacs" <&&> title =? "emacs-anywhere" -?> centerFloat 0.5 0.5
  , className =? "wiremix" -?> doFullCenterFloat
  , className =? "kew" -?> doFullCenterFloat
  , className =? "btm" -?> doFullCenterFloat
  , className =? "Qalculate-gtk" -?> doFullCenterFloat
  , className =? "PrismLauncher" -?> doFullCenterFloat <> doShift "5"
  , className =^ "Minecraft" -?> doFullCenterFloat <> doShift "5"
  , className =? "Dragon-drop" -?> doCenterFloat
  , className =? "mpv" -?> tileNormal
  , className =? "Pinentry" -?> doCenterFloat
  , className =? "pinentry-gtk-2" -?> doCenterFloat
  , className =? "Steam" <&&> not <$> title =? "Steam" -?> doCenterFloat
  , className =? "qemu-system-x86" -?> doCenterFloat
  , className =? "qemu-system-x86_64" -?> doCenterFloat
  , isRole =? "GtkFileChooserDialog" -?> doCenterFloat
  , isRole =? "pop-up" -?> doCenterFloat
  , isRole =? "About" -?> doCenterFloat
  , isDialog -?> doCenterFloat
  , isRole =? "browser" -?> ewmhDesktopsManageHook
  , transience
  ]
  where
    tileNormal = insertPosition Above Newer
    tileBelow = insertPosition Below Newer
    tileBelowNoFocus = insertPosition Below Older
    doFullCenterFloat = centerFloat 0.8 0.8
    noBorder = hasBorder False

manageHook :: ManageHook
manageHook =
  composeAll
    [ composeOne composeActions
    , namedScratchpadManageHook scratchpads
    ]
