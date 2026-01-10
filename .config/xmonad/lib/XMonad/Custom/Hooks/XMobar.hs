module XMonad.Custom.Hooks.XMobar
  ( barSpawner
  )
where

import Control.Monad
import Data.Maybe
import Text.Printf
import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Util.ClickableWorkspaces

import XMonad.Custom.Hooks.Log

barCommand :: Int -> String -> String
barCommand = printf command
  where
    command = "xmobar -x %d $HOME/.config/xmonad/xmobarrc/%s.hs"

xmobarTop :: Int -> StatusBarConfig
xmobarTop screen = statusBarPropTo "_XMONAD_LOG_1" (barCommand screen "top") topBarPP'

xmobarBot :: Int -> StatusBarConfig
xmobarBot screen = statusBarPropTo "_XMONAD_LOG_2" (barCommand screen "bot") (pure botBarPP)

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner 0 = pure $ xmobarTop 0 <> xmobarBot 0 -- two bars and tray on the main screen
barSpawner _ = pure $ xmobarTop 1 -- only top bar on the rest of the screens
