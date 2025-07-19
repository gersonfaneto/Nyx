{
  config,
  lib,
  pkgs,
  ...
}: let
  unstable = import <nixos-unstable> {
    config.allowUnfree = true;
  };
in {
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

  users.users.gerson = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = ["audio" "video" "wheel" "docker" "networkmanager"];
    packages = with pkgs; [];
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  programs = {
    fish = {
      enable = true;
    };
    firefox = {
      enable = true;
    };
    direnv = {
      enable = true;
      enableFishIntegration = true;
    };
    nix-ld = {
      enable = true;
      libraries = with pkgs; [];
    };
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
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

  environment.systemPackages = with pkgs;
    [
      alacritty
      bluetui
      brightnessctl
      chafa
      diff-so-fancy
      emacs
      fastfetch
      fd
      feh
      ffmpeg
      file
      fish
      flameshot
      fzf
      gh
      git
      highlight
      htop
      jq
      less
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
    ]
    ++ [
      # Nyx
      nil
      alejandra

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
    ];

  fonts = {
    packages = with pkgs; [
      nerd-fonts.symbols-only
      noto-fonts-color-emoji
      (iosevka-bin.override {variant = "SS07";})
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = ["Noto Emoji"];
        serif = ["Iosevka SS07"];
        sansSerif = ["Iosevka SS07"];
        monospace = ["Iosevka SS07"];
      };
    };
  };

  system.stateVersion = "24.11"; # DON'T CHANGE THIS!
}
