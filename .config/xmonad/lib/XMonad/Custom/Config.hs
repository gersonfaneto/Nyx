{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module XMonad.Custom.Config
  ( mConfig
  )
where

import Flow
import System.IO (writeFile)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Actions.MostRecentlyUsed
import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Actions.Submap
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat
  , doRectFloat
  , isDialog
  )
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
import XMonad.Util.Loggers
import XMonad.Util.Loggers.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

import Data.Map qualified as M
import XMonad.StackSet qualified as S
import XMonad.Util.Hacks qualified as Hacks

import XMonad.Custom.Scratchpads

import XMonad.Custom.Actions.RecentWindows qualified as C
import XMonad.Custom.Actions.RecentWorkspaces qualified as C
import XMonad.Custom.Bindings qualified as C
import XMonad.Custom.Hooks.Event qualified as C
import XMonad.Custom.Hooks.Layout qualified as C
import XMonad.Custom.Hooks.Log qualified as C
import XMonad.Custom.Hooks.Screens qualified as C
import XMonad.Custom.Hooks.Startup qualified as C
import XMonad.Custom.Hooks.Statusbar qualified as C
import XMonad.Custom.Manage.ManageHook qualified as C
import XMonad.Custom.Misc qualified as C
import XMonad.Custom.MouseBindings qualified as C
import XMonad.Custom.Navigation qualified as C
import XMonad.Custom.Scratchpads qualified as C
import XMonad.Custom.Theme qualified as C
import XMonad.Custom.Workspaces qualified as C

mConfig =
  def
    { modMask = C.modMask
    , terminal = C.term C.applications
    , borderWidth = C.border
    , normalBorderColor = C.colorN
    , focusedBorderColor = C.colorF
    , keys = C.myKeys
    , workspaces = C.workspaces
    , logHook = C.logHook
    , manageHook = C.manageHook
    , layoutHook = C.layoutHook
    , startupHook = C.startupHook
    , handleEventHook = C.handleEventHook
    , mouseBindings = C.mouseBindings
    , clickJustFocuses = False
    , focusFollowsMouse = True
    }
    |> withNavigation2DConfig C.navigation
    |> addRandrChangeHook C.myRandrChangeHook
    |> dynamicProjects C.projects
    |> dynamicSBs C.barSpawner
    |> C.configureRecentWindows
    |> C.configureRecentWorkspaces
    |> ewmh
    |> ewmhFullscreen
    |> docks
    |> Hacks.javaHack
    |> (return :: a -> IO a)

mWorkspaces :: [String]
mWorkspaces = map show [1 .. 10]

mModMask :: KeyMask
mModMask = mod4Mask

mTerminal, mTerminal' :: String
mTerminal = "ghostty"
mTerminal' = "alacritty"

mTSystem :: String
mTSystem = mTerminal' <> " --class btm --command btm"

mTMusic :: String
mTMusic = mTerminal' <> " --class kew --command kew"

mTSound :: String
mTSound = mTerminal' <> " --class wiremix --command wiremix"

mBrowser, mBrowser' :: String
mBrowser = "qutebrowser"
mBrowser' = "firefox"

mEditor :: String
mEditor = "emacsclient --create-frame --alternate-editor 'emacs'"

mFiles :: String
mFiles = "nautilus"

mCalc :: String
mCalc = "qalculate-gtk"

setSubmap :: String -> X ()
setSubmap name = io $ writeFile "/home/gerson/.cache/xmonad-submap" name

submapP :: [(String, X ())] -> X ()
submapP =
  submap . mkKeymap def

namedSubmapP :: String -> [(String, X ())] -> X ()
namedSubmapP name ks = do
  setSubmap name
  submapP ks
  setSubmap ""

gameSubmap =
  namedSubmapP "Game" $
    [ ("m", spawn "prismlauncher")
    ]

toolSubmap =
  namedSubmapP "Tool" $
    [ ("z", spawn "boomer")
    , ("m", spawn mTSystem)
    , ("s", spawn mTSound)
    ]

openSubmap =
  namedSubmapP "Open" $
    [ ("e", spawn mEditor)
    , ("b", spawn mBrowser)
    , ("S-b", spawn mBrowser')
    , ("m", spawn mTMusic)
    , ("c", spawn mCalc)
    , ("f", spawn mFiles)
    , ("p", spawn "zathura")
    ]

systemSubmap =
  namedSubmapP "System" $
    [ ("s", spawn "silence")
    , ("c", spawn "caffeine")
    , ("b", spawn "background alt")
    , ("d", spawn "dunstctl close-all")
    , ("x", spawn "xmonad --recompile && xmonad --restart")
    ]

workspaceSubmap =
  namedSubmapP "Workspace" $
    [ ("t", sendMessage $ JumpToLayout "Tall")
    , ("w", sendMessage $ JumpToLayout "Wide")
    , ("f", sendMessage $ JumpToLayout "Full")
    , ("s", sendMessage $ JumpToLayout "Spiral")
    ]

getSortByIndexNonSP :: X ([WindowSpace] -> [WindowSpace])
getSortByIndexNonSP = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

nextNonEmptyWS, prevNonEmptyWS :: X ()
nextNonEmptyWS =
  findWorkspace getSortByIndexNonSP Next HiddenNonEmptyWS 1 >>= windows <. S.view
prevNonEmptyWS =
  findWorkspace getSortByIndexNonSP Prev HiddenNonEmptyWS 1 >>= windows <. S.view

mKeys =
  [ ("M-<Escape>", spawn "system")
  , ("M-<Space>", spawn "launcher")
  , ("M-<Print>", spawn "capture")
  , ("M-p", spawn "rofi-rbw")
  , ("M-<Return>", spawn mTerminal)
  , ("M-S-<Return>", spawn mTerminal')
  , ("M-g", gameSubmap)
  , ("M-t", toolSubmap)
  , ("M-o", openSubmap)
  , ("M-s", systemSubmap)
  , ("M-w", workspaceSubmap)
  , ("M-q", kill)
  , ("M-C-l", nextWS)
  , ("M-C-h", prevWS)
  , ("M-S-l", shiftToNext >> nextWS)
  , ("M-S-h", shiftToPrev >> prevWS)
  , ("M-<Tab>", toggleWS)
  , ("M-j", windows S.focusDown)
  , ("M-k", windows S.focusUp)
  , ("M-f", sequence_ [withFocused $ windows . S.sink, sendMessage $ Toggle NBFULL])
  , ("M-,", prevNonEmptyWS)
  , ("M-.", nextNonEmptyWS)
  , ("M-;", spawn "rofimoji")
  , ("M-z", incWindowSpacing 8)
  , ("M-x", decWindowSpacing 8)
  , ("M-a", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)
  , ("M-S-a", setWindowSpacing (Border 8 8 8 8) >> setScreenSpacing (Border 8 8 8 8))
  , ("<XF86AudioRaiseVolume>", spawn "volume up")
  , ("<XF86AudioLowerVolume>", spawn "volume down")
  , ("<XF86AudioMute>", spawn "volume mute")
  , ("M-<XF86AudioMute>", spawn "volume mute")
  , ("M-M1-k", spawn "volume up")
  , ("M-M1-j", spawn "volume down")
  , ("M-M1-m", spawn "volume mute")
  , ("M-M1-v", spawn "volume mute")
  , ("<XF86AudioPrev>", spawn "playerctl previous")
  , ("<XF86AudioNext>", spawn "playerctl next")
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("M-M1-h", spawn "playerctl previous")
  , ("M-M1-l", spawn "playerctl next")
  , ("M-M1-p", spawn "playerctl play-pause")
  , ("<XF86MonBrightnessDown>", spawn "brightness down")
  , ("<XF86MonBrightnessUp>", spawn "brightness up")
  ]
    ++ [ (mask ++ "M-" ++ [key], windows $ action tag)
       | (tag, key) <- zip mWorkspaces "1234567890"
       , (action, mask) <- [(S.greedyView, ""), (S.shift, "S-")]
       ]
