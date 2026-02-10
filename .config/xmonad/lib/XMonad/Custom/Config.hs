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
