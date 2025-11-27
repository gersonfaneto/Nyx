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
      # (pkgs.fetchpatch {
      #   url = "https://tools.suckless.org/dmenu/patches/center/dmenu-center-20240616-36c3d68.diff";
      #   hash = "sha256-sTDzNi6VRPddFcR9pPKcfP588ZwaYWRlk28ehjnR0xo=";
      # })
      (pkgs.fetchpatch {
        url = "https://tools.suckless.org/dmenu/patches/lines-below-prompt/dmenu-linesbelowprompt-and-fullwidth-20211014.diff";
        hash = "sha256-ZrnFJeeA4atZ2fwsJN15FLU8WhtCKZju790CgE19bks=";
      })
    ];
  });
in {
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
    ".config/gtk-3.0/" = mkSymlink ".config/gtk-3.0/";
    ".config/i3/" = mkSymlink ".config/i3/";
    ".config/i3status/" = mkSymlink ".config/i3status/";
    ".config/kew/" = mkSymlink ".config/kew/";
    ".config/mimeapps.list" = mkSymlink ".config/mimeapps.list";
    ".config/nvim/" = mkSymlink ".config/nvim/";
    ".config/pandoc/" = mkSymlink ".config/pandoc/";
    ".config/rg/" = mkSymlink ".config/rg/";
    ".config/rmpc/" = mkSymlink ".config/rmpc/";
    ".config/tmux/" = mkSymlink ".config/tmux/";
    ".config/zathura/" = mkSymlink ".config/zathura/";
    ".emacs.d/" = mkSymlink ".emacs.d/";
    ".gitconfig" = mkSymlink ".gitconfig";
    ".highlight/" = mkSymlink ".highlight/";
    ".local/share/backgrounds/" = mkSymlink ".local/share/backgrounds/";
  };

  home.packages = with pkgs; [
    mpc
    kew
    dmenu
  ];

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
