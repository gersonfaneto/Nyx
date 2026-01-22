{ pkgs, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  nixpkgs.overlays = [
    (final: prev: {
      qutebrowser = prev.qutebrowser.override {
        enableWideVine = true;
      };
    })
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

  nix.settings.trusted-users = [ "gerson" ];

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

  systemd = {
    user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
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
    packages = with pkgs; [ gnupg ];
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
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  programs.neovim = {
    enable = true;
    package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    extraPackages = with pkgs; [
      docker-compose
    ];
  };

  environment.variables = rec {
    XDG_BIN_HOME = "$HOME/.local/bin";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    PATH = [ "${XDG_BIN_HOME}" ];

    TERM = "alacritty";

    XSECURELOCK_AUTH_TIMEOUT = 5;
    XSECURELOCK_BLANK_DPMS_STATE = "suspend";
    XSECURELOCK_BLANK_TIMEOUT = 5;
  };

  environment.systemPackages =
    (with pkgs; [
      acpi
      alacritty
      alsa-utils
      ani-cli
      bat
      beets
      bluetui
      bottom
      brightnessctl
      cached-nix-shell
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
      fish
      fzf
      gh
      ghostty
      gifsicle
      git
      hyperfine
      imagemagick
      ispell
      jq
      less
      libnotify
      libqalculate
      libtool
      localsend
      lsof
      maim
      man-pages
      man-pages-posix
      mermaid-cli
      mpv
      nsxiv
      pandoc
      pinentry-gnome3
      playerctl
      polkit_gnome
      poppler-utils
      presenterm
      prismlauncher
      procps
      pulseaudio
      qalculate-gtk
      qutebrowser
      ranger
      rbw
      ripgrep
      rofi
      rofi-rbw
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
      xmobar
      xorg.xrandr
      xsecurelock
      xsel
      xss-lock
      yt-dlp
      zoxide
      # ...
      ghc
      ghcid
      cabal2nix
      cabal-install
      nix-prefetch-git
      haskellPackages.fourmolu
      haskellPackages.cabal-gild
      haskellPackages.haskell-language-server
    ])
    ++ (with pkgs; [
      (callPackage ./packages/boomer.nix { })
    ])
    ++ (with pkgs; [
      # Nix
      nil
      nixd
      nixpkgs-fmt

      # Markdown
      marksman

      # Vim
      vim-language-server

      # Lua
      lua
      stylua
      lua-language-server
      luajitPackages.luacheck

      # C
      gcc
      cmake
      gnumake

      # Clojure
      boot
      clojure
      clojure-lsp
      leiningen

      # Python
      python312
      python312Packages.uv
      python312Packages.pip
      python312Packages.pyqt6 # This is for `qutebrowser'

      # Javascript
      bun
      nodejs

      # Bash
      shfmt
      bash-language-server

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

      # Retro
      ibm-plex
      nerd-fonts.blex-mono

      # Rounded
      maple-mono.NF
      maple-mono.NF-CN
      maple-mono.truetype
      maple-mono.variable

      # Readable
      aporetic-bin
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = [ "Noto Emoji" ];
        serif = [ "Maple Mono NF CN" ];
        sansSerif = [ "Maple Mono NF CN" ];
        monospace = [ "Maple Mono NF CN" ];
      };
    };
  };

  system.stateVersion = "24.11"; # DON'T CHANGE THIS!
}
