module XMobar.Custom.Layout.Top
  ( topConfig,
  )
where

import XMobar.Custom.Layout.Base
import Xmobar

topConfig :: Config
topConfig =
  baseConfig
    { position = TopHM 30 10 10 10 0
    , commands =
        [ Run $ Date "%d.%m.%y / %A / %H:%M" "date" 10
        , Run $ ComX "caffeinatestatus" [] "" "caffeine" 20
        , Run $ XPropertyLog "_XMONAD_LOG_1"
        , Run $ Com "cat" ["/home/gerson/.cache/xmonad-submap"] "submap" 1
        ]
    , template =
        " %_XMONAD_LOG_1% } %submap% { %caffeine%  %date% "
    }
