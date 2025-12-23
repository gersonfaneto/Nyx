{
  lib,
  pkgs,
  config,
  inputs,
  ...
}: let
  colors = config.lib.stylix.colors.withHashtag;
  scheme = "${pkgs.base16-schemes}/share/themes/solarized-dark.yaml";
in {
  imports = [inputs.stylix.homeModules.stylix];

  stylix.enable = true;
  stylix.autoEnable = true;
  stylix.enableReleaseChecks = false;

  stylix.base16Scheme = scheme;

  stylix.polarity = "dark";

  stylix.targets.qt.enable = true;

  stylix.targets.gtk.enable = true;

  stylix.fonts = {
    serif = {
      package = pkgs.aporetic;
      name = "Aporetic Serif";
    };
    sansSerif = {
      package = pkgs.aporetic;
      name = "Aporetic Sans";
    };
    monospace = {
      package = pkgs.aporetic;
      name = "Aporetic Serif Mono";
    };
    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
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
    builtins.genList (x: {
      name = "STYLIX_BASE_0${lib.toHexString x}";
      value = colors."base0${lib.toHexString x}";
    })
    15
  );
}
