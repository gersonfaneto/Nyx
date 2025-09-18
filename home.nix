{inputs, ...}: {
  # ...
  home.username = "gerson";
  home.homeDirectory = "/home/gerson";

  # ...
  programs.home-manager.enable = true;

  home.packages = with inputs.nixpkgs; [
    # home-manager # Not needed, is managed by the flake...
  ];

  # FIX: This might cause some problems in the future...
  home.enableNixpkgsReleaseCheck = false;

  home.stateVersion = "25.05"; # DON'T CHANGE THIS!
}
