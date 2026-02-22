module XMonad.Custom.Hooks.Screens
  ( myRandrChangeHook
  )
where

import           XMonad.Actions.ShowText
import           XMonad.Core

flash' :: String -> X ()
flash' = flashText def 0.5

myRandrChangeHook :: X ()
myRandrChangeHook = do
  spawn "autorandr --change"
  spawn "~/.fehbg"
  flash' "Screen change"
