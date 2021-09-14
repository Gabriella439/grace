let
  pkgs = import <nixpkgs> { config = { allowBroken = true; }; overlays = []; };

in
  (pkgs.haskellPackages.callCabal2nix "grace" ./. { }).env
