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

  services.gnome.gnome-keyring.enable = true;

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

  programs.gnupg = {
    agent = {
      enable = true;
    };
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
      ani-cli
      arandr
      aseprite
      bat
      beets
      bluetui
      bottom
      brightnessctl
      cava
      chafa
      detach
      diff-so-fancy
      dig
      dmidecode
      dragon-drop
      dunst
      emacs30-gtk3
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
      highlight
      hyperfine
      imagemagick
      ispell
      jq
      kew
      killall
      less
      libnotify
      libqalculate
      libtool
      localsend
      lsof
      maim
      man-pages
      man-pages-posix
      mpv
      nsxiv
      pandoc
      playerctl
      poppler-utils
      presenterm
      prismlauncher
      procps
      pulseaudio
      qalculate-gtk
      qutebrowser
      qutebrowser
      rbw
      ripgrep
      rmpc
      rofi
      rsync
      scc
      sct
      slop
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

      # Bash
      shfmt
      bash-language-server

      # Node
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
      nerd-fonts.symbols-only

      # Emojis
      noto-fonts-color-emoji

      # General
      aporetic
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
