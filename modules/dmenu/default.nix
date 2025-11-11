{pkgs, ...}: {
  home.packages = [
    (pkgs.dmenu.overrideAttrs (_: {
      src = ./src;
      patches = [];
    }))
  ];
}
