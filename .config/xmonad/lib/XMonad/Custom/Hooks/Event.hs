module XMonad.Custom.Hooks.Event
  ( handleEventHook
  )
where

import           Data.Monoid
import           XMonad                                  hiding
                                                         (handleEventHook,
                                                          manageHook)
import           XMonad.Actions.ShowText
import           XMonad.Hooks.RefocusLast

import qualified XMonad.Util.Hacks                       as Hacks

import           XMonad.Custom.Hooks.KeyboardChangeEvent

myRefocusPred = refocusingIsActive <||> isFloat

handleEventHook :: Event -> X All
handleEventHook =
  mconcat hooks
  where
    hooks =
      -- serverEventHooks ++
      [ -- floatConfReqHook myFloatConfReqHook,
        -- nspTrackHook scratchpads,
        handleTimerEvent
      , keyboardChangeEventHook
      , -- perWindowKbdLayout,
        refocusLastWhen myRefocusPred
      , Hacks.trayerAboveXmobarEventHook
      , Hacks.trayerPaddingXmobarEventHook
      -- Hacks.windowedFullscreenFixEventHook
      -- fixSteamFlicker,
      -- mconcat $ swallower <$> ["Alacritty", "St"]
      -- , onTitleChange manageHook
      ]
