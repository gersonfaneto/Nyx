{ pkgs
, config
, ...
}:
let
  NYX_PATH = "/home/gerson/Developer/Personal/Nyx";

  mkSymlink = path: {
    source = config.lib.file.mkOutOfStoreSymlink "${NYX_PATH}/${path}";
  };
in
{
  imports = [
    ./stylix.nix
  ];

  nixpkgs.overlays = [
    (final: prev: {
      dmenu = prev.stdenv.mkDerivation rec {
        pname = "dmenu-flexipatch";
        version = "5.3-20241115";

        src = prev.fetchFromGitHub {
          owner = "bakkeby";
          repo = "dmenu-flexipatch";
          rev = "master";
          sha256 = "sha256-vV4SkxUcq8fzTZQrMtas720kA6thhJ1/kFM0/On1MQU=";
        };

        buildInputs = with prev; [
          xorg.libX11
          xorg.libXft
          xorg.libXinerama
          fontconfig
        ];
        nativeBuildInputs = with prev; [ pkg-config ];

        postPatch = ''
          # Copy the default patches configuration
          cp patches.def.h patches.h

          # Enable XYW patch
          sed -i 's/#define XYW_PATCH 0/#define XYW_PATCH 1/' patches.h
          sed -i 's/#define CENTER_PATCH 0/#define CENTER_PATCH 1/' patches.h
          sed -i 's/#define CTRL_V_TO_PASTE_PATCH 0/#define CTRL_V_TO_PASTE_PATCH 1/' patches.h
          sed -i 's/#define FUZZYMATCH_PATCH 0/#define FUZZYMATCH_PATCH 1/' patches.h
          sed -i 's/#define HIGHLIGHT_PATCH 0/#define HIGHLIGHT_PATCH 1/' patches.h
        '';

        makeFlags = [ "PREFIX=$(out)" ];

        meta = with prev.lib; {
          description = "dmenu with flexipatch - includes xyw and many other patches";
          homepage = "https://github.com/bakkeby/dmenu-flexipatch";
          license = licenses.mit;
          platforms = platforms.all;
        };
      };
    })
  ];

  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  home.file = {
    ".bin/" = mkSymlink ".bin/";
    ".config/alacritty/" = mkSymlink ".config/alacritty/";
    ".config/beets/" = mkSymlink ".config/beets/";
    ".config/dunst/" = mkSymlink ".config/dunst/";
    ".config/fastfetch/" = mkSymlink ".config/fastfetch/";
    ".config/fd/" = mkSymlink ".config/fd/";
    ".config/feh/" = mkSymlink ".config/feh/";
    ".config/fish/" = mkSymlink ".config/fish/";
    ".config/ghostty/" = mkSymlink ".config/ghostty/";
    ".config/nvim/" = mkSymlink ".config/nvim/";
    ".config/pandoc/" = mkSymlink ".config/pandoc/";
    ".config/qutebrowser/" = mkSymlink ".config/qutebrowser/";
    ".config/rg/" = mkSymlink ".config/rg/";
    ".config/rofi/" = mkSymlink ".config/rofi/";
    ".config/tmux/" = mkSymlink ".config/tmux/";
    ".config/zathura/" = mkSymlink ".config/zathura/";
    ".config/xmonad/" = mkSymlink ".config/xmonad/";
    ".config/mimeapps.list" = mkSymlink ".config/mimeapps.list";
    ".emacs.d/" = mkSymlink ".emacs.d/";
    ".local/share/wallpapers/" = mkSymlink ".local/share/wallpapers/";
    ".gitconfig" = mkSymlink ".gitconfig";
    ".bashrc" = mkSymlink ".bashrc";
    ".profile" = mkSymlink ".profile";
    ".bash_profile" = mkSymlink ".bash_profile";
  };

  home.packages = with pkgs; [
    kew
    zathura
    nautilus
  ];

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.picom = {
    enable = true;
    backend = "glx";
    shadow = true;

    shadowOpacity = 0.75;
    shadowOffsets = [ (-5) (-5) ];
    shadowExclude = [
      "! name~=''"
      "name = 'xmobar'"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    inactiveOpacity = 1.0;
    activeOpacity = 1.0;

    fade = true;
    fadeDelta = 4;
    fadeSteps = [ 0.33 0.03 ];

    vSync = false;

    wintypes = {
      "tooltip" = {
        fade = true;
        shadow = false;
        opacity = 0.85;
        focus = true;
      };
    };

    settings = {
      # corner-radius = 12;

      shadow-radius = 12;
      xinerama-shadow-crop = true;

      frame-opacity = 1;
      inactive-opacity-override = false;
      detect-client-opacity = true;

      blur-backgroupnd = true;
      blur-background-frame = true;
      blur-background-fixed = false;
      blur-background-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
      ];

      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      use-ewmh-active-win = true;
      detect-rounded-corners = true;

      dbe = false;
      unredir-if-possible = false;
      detect-transient = true;
      detect-client-leader = true;

      xrender-sync-fence = true;
    };
  };

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
