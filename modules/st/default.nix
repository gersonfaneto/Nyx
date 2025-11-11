{pkgs, ...}: {
  home.packages = [
    (pkgs.st.overrideAttrs (_: {
      src = ./src;
      patches = [];
    }))
  ];
}
