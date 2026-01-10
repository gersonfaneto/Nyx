module XMobar.Custom.Layout.Bottom (
  botConfig,
) where

import Xmobar

import XMobar.Custom.Layout.Base

botConfig :: Config
botConfig =
  baseConfig
    { position = BottomHM 30 10 10 0 10
    , commands =
        [ Run $ ComX "player" [] "" "player" 20
        , Run $ ComX "dunststatus" [] "" "dunst" 20
        , Run $
            Volume
              "default"
              "Master"
              [ "-t"
              , "<fn=1><status></fn><volume>%"
              , "--"
              , "--on"
              , "\984446"
              , "--off"
              , "\984449"
              , "--onc"
              , "#a19782"
              , "--offc"
              , "#a19782"
              ]
              10
        , Run $
            Battery
              [ "-t"
              , "<fn=3><acstatus></fn> <left>%"
              , "--"
              , "-i"
              , "\62016"
              , "-O"
              , "\61671"
              , "-o"
              , "\62018"
              ]
              10
        ]
    , template =
        " %battery% } %player% { %dunst%  %default:Master% "
    }
