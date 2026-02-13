{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module XMonad.Custom.Config
  ( mConfig
  )
where

import           Flow
import           System.IO                              (writeFile)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects         (dynamicProjects)
import           XMonad.Actions.MostRecentlyUsed
import           XMonad.Actions.Navigation2D            (withNavigation2DConfig)
import           XMonad.Actions.Submap
import           XMonad.Core
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers             (doCenterFloat,
                                                         doRectFloat, isDialog)
import           XMonad.Hooks.Rescreen
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Loggers.NamedScratchpad
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties
import           XMonad.Util.WorkspaceCompare

import qualified Data.Map                               as M
import qualified XMonad.StackSet                        as S
import qualified XMonad.Util.Hacks                      as Hacks

import           XMonad.Custom.Scratchpads

import qualified XMonad.Custom.Actions.RecentWindows    as C
import qualified XMonad.Custom.Actions.RecentWorkspaces as C
import qualified XMonad.Custom.Bindings                 as C
import qualified XMonad.Custom.Hooks.Event              as C
import qualified XMonad.Custom.Hooks.Layout             as C
import qualified XMonad.Custom.Hooks.Log                as C
import qualified XMonad.Custom.Hooks.Screens            as C
import qualified XMonad.Custom.Hooks.Startup            as C
import qualified XMonad.Custom.Hooks.Statusbar          as C
import qualified XMonad.Custom.Manage.ManageHook        as C
import qualified XMonad.Custom.Misc                     as C
import qualified XMonad.Custom.MouseBindings            as C
import qualified XMonad.Custom.Navigation               as C
import qualified XMonad.Custom.Scratchpads              as C
import qualified XMonad.Custom.Theme                    as C
import qualified XMonad.Custom.Workspaces               as C

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
