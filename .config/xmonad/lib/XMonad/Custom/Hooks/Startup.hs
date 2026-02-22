module XMonad.Custom.Hooks.Startup
  ( startupHook
  )
where

import           XMonad                hiding (startupHook)
import           XMonad.Util.SpawnOnce (spawnOnce)

startupHook :: X ()
startupHook = do
  spawnOnce "dunst -config $HOME/.config/dunst/dunstrc"
  spawnOnce "feh --bg-fill $HOME/.local/share/wallpapers/1920x1080.jpg"
  spawnOnce "battery -L 20 -n"
  spawnOnce "xset s 600"
  spawnOnce "xset r rate 200 100"
  spawnOnce "xss-lock -l -- xsecurelock"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "setxkbmap -option ctrl:nocaps -layout br -variant thinkpad"
