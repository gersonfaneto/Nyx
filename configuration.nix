{
  # lib,
  # inputs,
  # config,
  nyx,
  pkgs,
  pkgs-unstable,
  apple-fonts,
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

  nix.settings.trusted-users = [
    "${nyx.user}"
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "Nyx";
  networking.networkmanager.enable = true;

  systemd.services.NetworkManager-wait-online.enable = false;

  time.timeZone = "America/Bahia";

  services = {
    xserver = {
      enable = true;
      windowManager = {
        i3 = {
          enable = true;
        };
      };
    };
  };

  services = {
    displayManager = {
      ly = {
        enable = true;
      };
    };
  };

  services = {
    xserver = {
      xkb = {
        layout = "br";
        variant = "thinkpad";
      };
    };
  };

  services.pipewire = {
    enable = true;
    pulse.enable = true;
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

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
    config.common.default = ["gtk"];
  };

  users.users.${nyx.user} = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = ["audio" "video" "wheel" "docker" "networkmanager"];
    # packages = with pkgs; [];
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

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
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

    XSECURELOCK_BLANK_TIMEOUT = 5;
    XSECURELOCK_AUTH_TIMEOUT = 5;
    XSECURELOCK_BLANK_DPMS_STATE = "suspend";
  };

  environment.systemPackages =
    (with pkgs; [
      alacritty
      ani-cli
      arandr
      aseprite
      bat
      bluetui
      bottom
      brightnessctl
      chafa
      detach
      diff-so-fancy
      dunst
      emacs
      fastfetch
      fd
      feh
      ffmpeg
      file
      firefox
      fish
      flameshot
      fzf
      gh
      ghostty
      git
      highlight
      htop
      jq
      lazydocker
      lazygit
      lazysql
      less
      libnotify
      libqalculate
      man-pages
      man-pages-posix
      mpv
      neovim
      pandoc
      playerctl
      poppler-utils
      presenterm
      prismlauncher
      pulseaudio
      pulsemixer
      qalculate-gtk
      ripgrep
      rsync
      scc
      stow
      texlive.combined.scheme-full
      tmux
      trash-cli
      tree
      tree-sitter
      unzip
      vim
      vlc
      w3m
      wget
      xclip
      xdragon
      xsecurelock
      xsel
      xss-lock
      zathura
      zoxide
    ])
    ++ (with pkgs-unstable; [
      opencode
    ])
    ++ (with pkgs.libsForQt5; [
      full
      okular
    ])
    ++ (with pkgs; [
      # Nyx
      nil
      alejandra

      # Markdown
      marksman

      # Lua
      lua
      stylua
      lua-language-server
      luajitPackages.luacheck

      # C
      gcc
      gnumake
      cmake
      gdb
      strace
      valgrind

      # Python
      python312
      python312Packages.uv
      python312Packages.pip

      # Bash
      shfmt
      bash-language-server

      # Node
      nodejs_22

      # JSON
      vscode-langservers-extracted

      # General
      efm-langserver
    ]);

  fonts = {
    packages = with pkgs; [
      # Symbols
      nerd-fonts.symbols-only

      # Emojis
      noto-fonts-color-emoji

      # Coding
      mononoki

      # UI
      apple-fonts.sf-mono
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = ["Noto Emoji"];
        serif = ["SF Mono"];
        sansSerif = ["SF Mono"];
        monospace = ["SF Mono"];
      };
    };
  };

  system.stateVersion = "24.11"; # DON'T CHANGE THIS!
}
