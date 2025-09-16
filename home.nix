{pkgs, ...}: {
  # ...
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  # ...
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # home-manager # Not needed, is managed by the flake...
    neovim
  ];

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
