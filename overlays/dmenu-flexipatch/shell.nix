{ pkgs ? import <nixpkgs> { } }:
let
  dmenu-overlay = import ./default.nix;
  extended-packages = pkgs.appendOverlays [ dmenu-overlay ];
in
extended-packages.mkShell {
  buildInputs = [
    extended-packages.dmenu
  ];
}
