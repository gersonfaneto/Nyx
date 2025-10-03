{
  pkgs,
  inputs,
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
  networking.wireless.iwd.enable = true;
  # networking.networkmanager.enable = true;

  security.polkit = {
    enable = true;
  };

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

  users.users.gerson = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = ["audio" "video" "wheel" "docker" "networkmanager"];
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

  programs.gnupg = {
    agent = {
      enable = true;
    };
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
      gum
      highlight
      impala
      jq
      lazydocker
      lazygit
      lazysql
      less
      libnotify
      libqalculate
      localsend
      man-pages
      man-pages-posix
      mpv
      opencode
      pandoc
      playerctl
      poppler-utils
      presenterm
      prismlauncher
      pulseaudio
      pywal
      qalculate-gtk
      quartus-prime-lite
      ripgrep
      rsync
      scc
      stow
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
      xdragon
      xsecurelock
      xsel
      xss-lock
      zathura
      zoxide
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
    ])
    ++ (with pkgs; [
      python312Packages.pynvim
      python312Packages.ipykernel
      python312Packages.jupyter-client

      cairosvg
      python312Packages.kaleido
      python312Packages.nbformat
      python312Packages.plotly
      python312Packages.pnglatex
      python312Packages.pyperclip
      python312Packages.pyperclip
      python312Packages.pyqt6
    ]);

  fonts = {
    packages = with pkgs; [
      # Symbols
      nerd-fonts.symbols-only

      # Emojis
      noto-fonts-color-emoji

      # Coding
      bqn386
      miracode
      mononoki
      comic-mono
      departure-mono
      (iosevka-bin.override {variant = "SS07";})

      # UI
      inputs.apple-fonts.packages.${pkgs.system}.sf-mono
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
