{
  config,
  pkgs,
  ...
}: let
  NYX_PATH = "/home/gerson/Developer/Personal/Nyx";

  mkSymlink = path: {
    source = config.lib.file.mkOutOfStoreSymlink "${NYX_PATH}/${path}";
  };

  dmenu = pkgs.dmenu.overrideAttrs (old: {
    patches = [
      (pkgs.fetchpatch {
        url = "https://tools.suckless.org/dmenu/patches/lines-below-prompt/dmenu-linesbelowprompt-and-fullwidth-20211014.diff";
        hash = "sha256-ZrnFJeeA4atZ2fwsJN15FLU8WhtCKZju790CgE19bks=";
      })
      # (pkgs.fetchpatch {
      #   url = "https://tools.suckless.org/dmenu/patches/center/dmenu-center-20240616-36c3d68.diff";
      #   hash = "sha256-sTDzNi6VRPddFcR9pPKcfP588ZwaYWRlk28ehjnR0xo=";
      # })
      # (pkgs.fetchpatch {
      #   url = "https://tools.suckless.org/dmenu/patches/png_images/dmenu-png-images-5.3.diff";
      #   hash = "sha256-HjgdM6peQwYUhV3QEX7JEw3bvCSvzJ1Bd2naHxNUPy0=";
      # })
    ];
    buildInputs = old.buildInputs ++ [pkgs.libspng];
  });
in {
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  home.file = {
    ".config/alacritty/" = mkSymlink ".config/alacritty/";
    ".config/beets/" = mkSymlink ".config/beets/";
    ".config/doom/" = mkSymlink ".config/doom/";
    ".config/dunst/" = mkSymlink ".config/dunst/";
    ".config/fastfetch/" = mkSymlink ".config/fastfetch/";
    ".config/fd/" = mkSymlink ".config/fd/";
    ".config/feh/" = mkSymlink ".config/feh/";
    ".config/fish/" = mkSymlink ".config/fish/";
    ".config/ghostty/" = mkSymlink ".config/ghostty/";
    ".config/gtk-3.0/" = mkSymlink ".config/gtk-3.0/";
    ".config/i3/" = mkSymlink ".config/i3/";
    ".config/i3status/" = mkSymlink ".config/i3status/";
    ".config/kew/" = mkSymlink ".config/kew/";
    ".config/mimeapps.list" = mkSymlink ".config/mimeapps.list";
    ".config/nvim/" = mkSymlink ".config/nvim/";
    ".config/pandoc/" = mkSymlink ".config/pandoc/";
    ".config/rg/" = mkSymlink ".config/rg/";
    ".config/rofi/" = mkSymlink ".config/rofi/";
    ".config/tmux/" = mkSymlink ".config/tmux/";
    ".config/zathura/" = mkSymlink ".config/zathura/";
    ".config/xmobar/" = mkSymlink ".config/xmobar/";
    ".config/xmonad/" = mkSymlink ".config/xmonad/";
    ".bin/" = mkSymlink ".bin/";
    ".emacs.d/" = mkSymlink ".emacs.d/";
    ".highlight/" = mkSymlink ".highlight/";
    ".local/share/backgrounds/" = mkSymlink ".local/share/backgrounds/";
    ".local/share/applications/" = mkSymlink ".local/share/applications/";
    ".gitconfig" = mkSymlink ".gitconfig";
    ".bashrc" = mkSymlink ".bashrc";
    ".profile" = mkSymlink ".profile";
    ".bash_profile" = mkSymlink ".bash_profile";
  };

  home.packages = with pkgs; [
    mpc
    kew
    dmenu
  ];

  services.picom = {
    enable = true;
    backend = "glx";
    shadow = true;

    shadowOpacity = 0.75;
    shadowOffsets = [(-5) (-5)];
    shadowExclude = [
      "! name~=''"
      "name = 'Notification'"
      "name = 'Plank'"
      "name = 'Docky'"
      "name = 'Kupfer'"
      "name = 'xfce4-notifyd'"
      "name = 'cpt_frame_window'"
      "name *= 'VLC'"
      "name *= 'compton'"
      "name *= 'picom'"
      "name *= 'Chromium'"
      "name *= 'Chrome'"
      "class_g = 'Firefox' && argb"
      "class_g = 'Conky'"
      "class_g = 'Kupfer'"
      "class_g = 'Synapse'"
      "class_g ?= 'Notify-osd'"
      "class_g ?= 'Cairo-dock'"
      "class_g ?= 'Xfce4-notifyd'"
      "class_g ?= 'Xfce4-power-manager'"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    inactiveOpacity = 1.0;
    activeOpacity = 1.0;

    fade = true;
    fadeDelta = 4;
    fadeSteps = [0.33 0.03];

    vSync = true;

    wintypes = {
      "tooltip" = {
        fade = true;
        shadow = false;
        opacity = 0.85;
        focus = true;
      };
    };

    settings = {
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

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/home/gerson/Music";
    extraConfig = ''
      auto_update                "yes"
      restore_paused             "yes"

      log_file                   "syslog"
      pid_file                   "/tmp/mpd.pid"
      db_file                    "~/.config/mpd/mpd.db"
      state_file                 "~/.config/mpd/mpd.state"

      audio_output {
          type                   "pipewire"
          name                   "PipeWire Sound Server"
      }

      audio_output {
          type                   "fifo"
          name                   "Visualizer"
          format                 "44100:16:2"
          path                   "/tmp/mpd.fifo"
      }

      audio_output {
        type           "httpd"
        name           "lossless"
        encoder        "flac"
        port           "8000"
        max_clients     "8"
        mixer_type     "software"
        format         "44100:16:2"
      }
    '';
  };

  services.mpdris2 = {
    enable = true;
    notifications = true;
  };

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
