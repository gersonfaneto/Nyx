# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

let
  unstable = import <nixos-unstable> {
    config.allowUnfree = true;
  };
in

{
  imports =
    [ # Include the results of the hardware scan.
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

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "Nyx"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.
  systemd.services.NetworkManager-wait-online.enable = false; # This cuts boot time in half!

  # Set your time zone.
  time.timeZone = "America/Bahia";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    windowManager.i3.enable = true;
  };

  services.displayManager.ly = {
    enable = true;
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "br";
  services.xserver.xkb.variant = "thinkpad";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
    packages = with pkgs; [
      tree
    ];
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

  environment.variables = rec {
    XDG_CACHE_HOME               = "$HOME/.cache";
    XDG_CONFIG_HOME              = "$HOME/.config";
    XDG_DATA_HOME                = "$HOME/.local/share";
    XDG_STATE_HOME               = "$HOME/.local/state";
    XDG_BIN_HOME                 = "$HOME/.local/bin";

    PATH = [
      "${XDG_BIN_HOME}"
    ];

    XSECURELOCK_BLANK_TIMEOUT    = 5;
    XSECURELOCK_AUTH_TIMEOUT     = 5;
    XSECURELOCK_BLANK_DPMS_STATE = "suspend";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    bluetui
    brightnessctl
    chafa
    diff-so-fancy
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
  ] ++ ([
    # Lua
    lua
    stylua
    lua-language-server
    luajitPackages.luacheck

    # C
    gcc
    gnumake
    gdb
    strace
    valgrind

    # Python
    python312
    python312Packages.uv
    python312Packages.pip

    # pyright
    # python312Packages.ruff
    # python312Packages.pylint
    # python312Packages.flake8
    # python312Packages.mypy
    # python312Packages.isort

    # python312Packages.pynvim
    # python312Packages.jupyter-client
    # python312Packages.jupytext

    # Bash
    shfmt
    bash-language-server

    # Node
    nodejs_22

    # General
    efm-langserver
  ]);

  fonts = {
    packages = with pkgs; [
      nerd-fonts.symbols-only
      noto-fonts-color-emoji
      scientifica
      (iosevka-bin.override { variant = "SS07"; })
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = [ "Noto Emoji" ];
        serif = [  "Iosevka SS07"  ];
        sansSerif = [ "Iosevka SS07"  ];
        monospace = [ "Iosevka SS07" ];
      };
    };
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
    extraPackages = with pkgs; [
      docker-compose
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?
}
