module XMonad.Custom.Hooks.Startup
  ( startupHook
  )
where

import           Data.Maybe
import           XMonad                         hiding (startupHook)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Util.Cursor
import           XMonad.Util.SpawnOnce

import           XMonad.Custom.Actions.Keyboard

startupHook :: X ()
startupHook = do
  spawn "pkill dunst; dunst -config $HOME/.config/dunst/dunstrc &"
  spawn "feh --bg-fill $HOME/.local/share/wallpapers/1920x1080.jpg &"
  spawn "battery -L 20 -n &"
  spawn "xset s 600 &"
  spawn "xset r rate 200 100"
  spawn "xss-lock -l -- xsecurelock &"
  spawn "xsetroot -cursor_name left_ptr &"
  spawn "xrandr --output HDMI-1 --same-as eDP-1 &"
  spawn "setxkbmap -option ctrl:nocaps -layout br -variant thinkpad &"
  keyboardStartupHook
