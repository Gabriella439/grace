let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/e0ce3c683ae677cf5aab597d645520cddd13392b.tar.gz";
    sha256 = "08ans3w6r4fbs1wx6lzlp4xwhy6p04x3spvvrjz5950w8mzxqm61";
  };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          oldOverrides = old.overrides or (_: _: {});

          sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
            grace = ./grace;

            grace-core = ./grace-core;
          };

        in
          pkgsNew.lib.composeExtensions oldOverrides sourceOverrides;
    });
  };

  pkgs = import nixpkgs { config = { }; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskellPackages) grace-core grace; }
