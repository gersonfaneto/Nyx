Config
  { font = "Maple Mono NF CN 10",
    bgColor = "#0B0806",
    fgColor = "#A19782",
    alpha = 255,
    position = BottomHM 30 10 10 0 10,
    iconOffset = -1,
    lowerOnStart = True,
    hideOnStart = False,
    allDesktops = True,
    overrideRedirect = True,
    pickBroadest = False,
    persistent = True,
    border = FullBM 0,
    borderColor = "#A19782",
    borderWidth = 1,
    iconRoot = ".",
    commands =
      [ Run ComX "player" [] "" "player" 20
      , Run ComX "dunststatus" [] "" "dunst" 20
      , Run Volume "default" "Master" [ "-t", "<fn=1><status></fn><volume>%", "--", "--on", "󰕾 ", "--off", "󰖁 ", "--onc", "#a19782", "--offc", "#a19782" ] 10
      , Run Battery [ "-t", "<fn=3><acstatus></fn> <left>%", "--", "-i", "", "-O", "", "-o", "" ] 10
      ],
    sepChar = "%",
    alignSep = "}{",
    template = " %battery% } %player% { %dunst%  %default:Master% "
  }
