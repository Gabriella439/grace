let
  pkgs = import <nixpkgs> { config = {}; overlays = []; };

in
  (pkgs.haskellPackages.callCabal2nix "grace" ./. { }).env
