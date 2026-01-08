{ pkgs, config, ... }:
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
    ".config/rofimoji.rc" = mkSymlink ".config/rofimoji.rc";
    ".emacs.d/" = mkSymlink ".emacs.d/";
    ".local/share/wallpapers/" = mkSymlink ".local/share/wallpapers/";
    ".gitconfig" = mkSymlink ".gitconfig";
    ".bashrc" = mkSymlink ".bashrc";
    ".profile" = mkSymlink ".profile";
    ".bash_profile" = mkSymlink ".bash_profile";
  };

  home.packages = with pkgs; [
    bc
    kew
    zathura
    rofimoji
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
