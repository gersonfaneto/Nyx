{
  lib,
  pkgs,
  config,
  inputs,
  ...
}: let
  colors = config.lib.stylix.colors.withHashtag;
in {
  imports = [inputs.stylix.homeModules.stylix];

  stylix = {
    enable = true;
    enableReleaseChecks = false;
    autoEnable = true;

    base16Scheme = "${pkgs.base16-schemes}/share/themes/solarized-dark.yaml";

    targets = {
      gtk.enable = true;
    };
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
