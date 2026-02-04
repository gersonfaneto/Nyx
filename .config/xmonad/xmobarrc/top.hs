Config
    { font = "xft:Aporetic Sans Mono:style=Regular:size=8:antialias=true"
    , bgColor = "#0B0806"
    , fgColor = "#A19782"
    , alpha = 255
    , position = TopHM 30 10 10 10 0
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
    , commands =
        [ Run Date "%d.%m.%y / %A / %H:%M" "date" 10
        , Run ComX "caffeinatestatus" [] "" "caffeine" 20
        , Run XPropertyLog "_XMONAD_LOG_1"
        , Run Com "cat" ["/home/gerson/.cache/xmonad-submap"] "submap" 1
        ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = " %_XMONAD_LOG_1% } %submap% { %caffeinatestatus%  %date% "
    }
