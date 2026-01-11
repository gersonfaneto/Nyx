module XMobar.Custom.Layout.Base
  ( baseConfig
  )
where

import Xmobar

baseConfig :: Config
baseConfig =
  defaultConfig
    { font = "Aporetic Sans Mono 10"
    , bgColor = "#0B0806"
    , fgColor = "#A19782"
    , alpha = 255
    , iconOffset = -1
    , lowerOnStart = True
    , hideOnStart = False
    , allDesktops = True
    , overrideRedirect = True
    , pickBroadest = False
    , persistent = True
    , border = FullBM 0
    , borderColor = "#A19782"
    , borderWidth = 1
    , iconRoot = "."
    , sepChar = "%"
    , alignSep = "}{"
    }
