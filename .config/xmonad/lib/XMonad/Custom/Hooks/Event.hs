module XMonad.Custom.Hooks.Event
  ( handleEventHook
  )
where

import           Data.Monoid
import           XMonad                   hiding (handleEventHook, manageHook)
import           XMonad.Actions.ShowText
import           XMonad.Hooks.RefocusLast

import qualified XMonad.Util.Hacks        as Hacks

myRefocusPred :: Query Bool
myRefocusPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook =
  mconcat hooks
  where
    hooks =
      [ handleTimerEvent
      , refocusLastWhen myRefocusPred
      , Hacks.windowedFullscreenFixEventHook
      , Hacks.fixSteamFlicker
      ]
