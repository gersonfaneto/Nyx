Config
  { font = "IBM Plex Mono 10",
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
      [ Run Memory ["-t", "<fn=1> </fn> <usedratio>%"] 20,
        Run MultiCpu ["-t", "<fn=1>󰘚 </fn> <total>%"] 20,
        Run ComX "/home/gerson/.config/xmonad/scripts/xmobar/player" [] "" "player" 20,
        Run ComX "/home/gerson/.config/xmonad/scripts/xmobar/dunststatus" [] "" "dunst" 20,
        Run Volume "default" "Master" [ "-t", "<fn=1><status></fn><volume>%", "--", "--on", "󰕾 ", "--off", "󰖁 ", "--onc", "#a19782", "--offc", "#a19782" ] 10,
        Run Battery [ "-t", "<fn=3><acstatus></fn> <left>%", "--", "-i", "", "-O", "  ", "-o", "", "-a", "notify-send -u critical 'Battery running out!!!'", "-A", "5" ] 10
      ],
    sepChar = "%",
    alignSep = "}{",
    template = " %memory%  %multicpu% } %player% { %dunst%  %default:Master%  %battery% "
  }
