{
  pkgs,
  ...
}: {
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  programs.home-manager.enable = true;

  programs.nix-index.enable = true;
  programs.nix-index.enableFishIntegration = true;

  home.packages = with pkgs; [
    home-manager
  ];

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
