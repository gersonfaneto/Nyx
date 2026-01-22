{ lib, pkgs, config, inputs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
  scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
in
{
  imports = [ inputs.stylix.homeModules.stylix ];

  stylix.enable = true;
  stylix.autoEnable = true;
  stylix.enableReleaseChecks = false;

  stylix.base16Scheme = scheme;

  stylix.polarity = "dark";

  stylix.targets.qt.enable = true;

  stylix.targets.gtk.enable = true;

  stylix.fonts = {
    serif = {
      package = pkgs.ibm-plex;
      name = "Maple Mono NF CN";
    };
    sansSerif = {
      package = pkgs.ibm-plex;
      name = "Maple Mono NF CN";
    };
    monospace = {
      package = pkgs.ibm-plex;
      name = "Maple Mono NF CN";
    };
    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
    };
    sizes = {
      applications = 10; # Size for GUI applications
      desktop = 10; # Size for desktop environment elements
      popups = 10; # Size for notifications and menus
      terminal = 10; # Size for terminal/text editors
    };
  };

  stylix.cursor = {
    package = pkgs.numix-cursor-theme;
    name = "Numix-Cursor";
    size = 24;
  };

  stylix.icons = {
    enable = true;
    package = pkgs.numix-icon-theme-square;
    dark = "Numix-Square";
    light = "Numix-Square-Light";
  };

  # Declare all 16 base16 colors as session variables to be used by applications
  # not controlled by Home Manager.
  home.sessionVariables = builtins.listToAttrs (
    builtins.genList
      (x: {
        name = "STYLIX_BASE_0${lib.toHexString x}";
        value = colors."base0${lib.toHexString x}";
      })
      15
  );
}
