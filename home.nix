{
  inputs,
  config,
  ...
}: let
  NYX_PATH = "/home/gerson/Developer/Personal/Nyx";

  mkSymlink = path: {
    source = config.lib.file.mkOutOfStoreSymlink "${NYX_PATH}/${path}";
  };
in {
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  programs.home-manager.enable = true;

  home.packages = with inputs.pkgs; [];

  home.file = {
    ".bin/" = mkSymlink ".bin/";
    ".local/share/backgrounds/" = mkSymlink ".local/share/backgrounds/";
    ".highlight/" = mkSymlink ".highlight/";
    # ".emacs.d/" = mkSymlink ".emacs.d/";
    ".gitconfig" = mkSymlink ".gitconfig";
    ".config/alacritty/" = mkSymlink ".config/alacritty/";
    ".config/dunst/" = mkSymlink ".config/dunst/";
    ".config/doom/" = mkSymlink ".config/doom/";
    ".config/fastfetch/" = mkSymlink ".config/fastfetch/";
    ".config/fd/" = mkSymlink ".config/fd/";
    ".config/feh/" = mkSymlink ".config/feh/";
    ".config/fish/" = mkSymlink ".config/fish/";
    ".config/ghostty/" = mkSymlink ".config/ghostty/";
    ".config/gtk-3.0/" = mkSymlink ".config/gtk-3.0/";
    ".config/i3/" = mkSymlink ".config/i3/";
    ".config/i3status/" = mkSymlink ".config/i3status/";
    ".config/nvim/" = mkSymlink ".config/nvim/";
    ".config/opencode/" = mkSymlink ".config/opencode/";
    ".config/pandoc/" = mkSymlink ".config/pandoc/";
    ".config/rg/" = mkSymlink ".config/rg/";
    ".config/tmux/" = mkSymlink ".config/tmux/";
    ".config/zathura/" = mkSymlink ".config/zathura/";
    ".config/mimeapps.list" = mkSymlink ".config/mimeapps.list";
  };

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
