{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import XMobar.Custom.Layout.Bottom
import XMobar.Custom.Layout.Top
import Xmobar

main :: IO ()
main =
  getArgs >>= \case
    ("top" : _) -> xmobar topConfig
    ("bot" : _) -> xmobar botConfig
    _ -> xmobar topConfig -- default
