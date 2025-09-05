{pkgs, ...}: {
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  programs.home-manager.enable = true;

  programs.nix-index.enable = true;
  programs.nix-index.enableFishIntegration = true;

  home.packages = with pkgs; [
    emacs30
  ];

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  services.emacs = {
    enable = true;
    package = pkgs.emacs30;
  };

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
