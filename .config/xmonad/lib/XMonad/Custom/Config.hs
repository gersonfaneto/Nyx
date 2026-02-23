{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module XMonad.Custom.Config
  ( mConfig
  )
where

import           Flow
import           XMonad
import           XMonad.Actions.DynamicProjects  (dynamicProjects)
import           XMonad.Actions.Navigation2D     (withNavigation2DConfig)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Rescreen
import           XMonad.Hooks.StatusBar

import qualified XMonad.Util.Hacks               as Hacks

import qualified XMonad.Custom.Bindings          as C
import qualified XMonad.Custom.Hooks.Event       as C
import qualified XMonad.Custom.Hooks.Layout      as C
import qualified XMonad.Custom.Hooks.Log         as C
import qualified XMonad.Custom.Hooks.Screens     as C
import qualified XMonad.Custom.Hooks.Startup     as C
import qualified XMonad.Custom.Hooks.Statusbar   as C
import qualified XMonad.Custom.Manage.ManageHook as C
import qualified XMonad.Custom.Misc              as C
import qualified XMonad.Custom.MouseBindings     as C
import qualified XMonad.Custom.Navigation        as C
import qualified XMonad.Custom.Theme             as C
import qualified XMonad.Custom.Workspaces        as C

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
    |> ewmh
    |> ewmhFullscreen
    |> docks
    |> Hacks.javaHack
    |> (return :: a -> IO a)
