{ pkgs, inputs, ... }:
{
  imports = [ inputs.stylix.homeModules.stylix ];

  stylix.enable = true;
  stylix.autoEnable = true;
  stylix.enableReleaseChecks = false;

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";

  stylix.polarity = "dark";

  stylix.targets.qt.enable = true;

  stylix.targets.gtk.enable = true;

  stylix.fonts = {
    serif = {
      package = pkgs.fantasque-sans-mono;
      name = "Fantasque Sans Mono";
    };
    sansSerif = {
      package = pkgs.fantasque-sans-mono;
      name = "Fantasque Sans Mono";
    };
    monospace = {
      package = pkgs.fantasque-sans-mono;
      name = "Fantasque Sans Mono";
    };
    emoji = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Color Emoji";
    };
    sizes = {
      applications = 10;
      desktop = 10;
      popups = 10;
      terminal = 10;
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
}
