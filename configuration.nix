{
  pkgs,
  overlays,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  documentation = {
    dev = {
      enable = true;
    };
    man = {
      generateCaches = true;
    };
    nixos = {
      includeAllModules = true;
    };
  };

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  nix.settings.trusted-users = ["gerson"];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "Nyx";
  networking.networkmanager.enable = true;

  security.polkit = {
    enable = true;
  };

  systemd.services.NetworkManager-wait-online.enable = false;

  time.timeZone = "America/Bahia";

  services.xserver = {
    enable = true;
    autoRepeatDelay = 200;
    autoRepeatInterval = 35;
  };

  services.xserver.windowManager = {
    xmonad = {
      enable = true;
      enableContribAndExtras = true;
      enableConfiguredRecompile = true;
    };
  };

  services.displayManager = {
    ly = {
      enable = true;
    };
  };

  services.xserver = {
    xkb = {
      layout = "br";
      variant = "thinkpad";
    };
  };

  security.rtkit = {
    enable = true;
  };

  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse = {
      enable = true;
    };
  };

  services.syncthing = {
    enable = true;
    group = "users";
    user = "gerson";
    dataDir = "/home/gerson/.local/share/Syncthing";
    configDir = "/home/gerson/.config/syncthing";
    overrideDevices = false;
    overrideFolders = false;
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  services.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
    };
  };

  powerManagement.enable = true;
  services.thermald.enable = true;
  services.power-profiles-daemon.enable = true;

  services.logind.lidSwitch = "suspend";
  services.logind.lidSwitchExternalPower = "lock";
  services.logind.lidSwitchDocked = "ignore";

  services.tlp = {
    enable = false;
    settings = {
      CPU_DRIVER_OPMODE_ON_BAT = "active"; # passive caps 400mhz
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
      PLATFORM_PROFILE_ON_BAT = "balanced";
      PLATFORM_PROFILE_ON_AC = "performance";
      RUNTIME_PM_ON_AC = "auto";
      RUNTIME_PM_ON_BAT = "auto";
      WIFI_PWR_ON_BAT = "off";
      WIFI_PWR_ON_AC = "off";
      NMI_WATCHDOG = "1";
    };
  };

  services.gnome.gnome-keyring.enable = true;

  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };
  };

  services.fprintd = {
    enable = true;
    tod = {
      enable = true;
      driver = pkgs.libfprint-2-tod1-goodix;
    };
  };

  security.pam.services = {
    ly = {
      text = ''
        auth       substack     login
        account    include      login
        password   substack     login
        session    include      login
      '';
      enableGnomeKeyring = true;
    };
    login = {
      fprintAuth = true;
      enableGnomeKeyring = true;
    };
    xsecurelock = {
      fprintAuth = true;
      enableGnomeKeyring = true;
    };
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal
        xdg-desktop-portal-gtk
      ];
      config.common.default = "*";
    };
  };

  users.users.gerson = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [
      "audio"
      "video"
      "wheel"
      "docker"
      "networkmanager"
    ];
    packages = with pkgs; [gnupg];
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  programs.fish = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };

  programs.nix-ld = {
    enable = true;
  };

  programs.dconf = {
    enable = true;
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-curses;
  };

  programs.neovim = {
    enable = true;
    package = overlays.neovim.default;
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    extraPackages = with pkgs; [
      docker-compose
    ];
  };

  environment.variables = rec {
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    XDG_BIN_HOME = "$HOME/.local/bin";

    PATH = [
      "${XDG_BIN_HOME}"
    ];

    TERM = "alacritty";

    XSECURELOCK_BLANK_TIMEOUT = 5;
    XSECURELOCK_AUTH_TIMEOUT = 5;
    XSECURELOCK_BLANK_DPMS_STATE = "suspend";
  };

  environment.systemPackages =
    (with pkgs; [
      acpi
      alacritty
      alsa-utils
      bat
      beets
      bluetui
      bottom
      brightnessctl
      chafa
      diff-so-fancy
      dragon-drop
      dunst
      emacs-gtk
      fastfetch
      fd
      feh
      ffmpeg-full
      file
      firefox
      fish
      fprintd
      fzf
      gh
      ghostty
      gifsicle
      git
      hyperfine
      imagemagick
      jq
      less
      libnotify
      libqalculate
      localsend
      lsof
      maim
      man-pages
      man-pages-posix
      mpv
      nsxiv
      pandoc
      playerctl
      polkit_gnome
      poppler-utils
      presenterm
      prismlauncher
      procps
      pulseaudio
      qalculate-gtk
      qutebrowser
      ripgrep
      rofi
      rsync
      scc
      sct
      tmux
      trash-cli
      tree
      tree-sitter
      unzip
      vim
      vlc
      w3m
      wget
      wiremix
      xclip
      xcolor
      xdo
      xdotool
      xorg.xrandr
      xsecurelock
      xsel
      xss-lock
      yt-dlp
      zoxide
    ])
    ++ (with pkgs; [
      xmobar
      (haskellPackages.ghcWithPackages (self:
        with self; [
          xmonad
          xmonad-extras
          xmonad-contrib
        ]))
    ])
    ++ (with pkgs; [
      (callPackage ./packages/boomer.nix {})
    ])
    ++ (with pkgs; [
      # Nyx
      nixd
      alejandra

      # Markdown
      marksman

      # Lua
      lua
      stylua
      lua-language-server
      luajitPackages.luacheck

      # C
      bear
      clang-tools
      gcc
      gnumake
      cmake
      gdb
      strace
      valgrind

      # Haskell
      ghcid
      stack
      cabal-install
      ormolu
      fourmolu
      stylish-haskell
      haskell-language-server

      # Python
      python312
      python312Packages.uv
      python312Packages.pip
      python312Packages.pyqt6 # This is for `qutebrowser'

      # Bash
      shfmt
      bash-language-server

      # Node
      bun
      nodejs_22

      # HTML + CSS + JSON
      vscode-langservers-extracted

      # LaTeX
      texlive.combined.scheme-full
      texlab
      tectonic

      # Typst
      typst
      tinymist

      # General
      efm-langserver
    ]);

  fonts = {
    packages = with pkgs; [
      # Symbols
      font-awesome
      material-icons
      nerd-fonts.symbols-only

      # Emojis
      noto-fonts-color-emoji

      # General
      aporetic

      # Terminal
      nerd-fonts.terminess-ttf
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = ["Noto Emoji"];
        serif = ["Aporetic Serif"];
        sansSerif = ["Aporetic Sans"];
        monospace = ["Aporetic Serif Mono"];
      };
    };
  };

  system.stateVersion = "24.11"; # DON'T CHANGE THIS!
}
