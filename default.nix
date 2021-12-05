let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/391f93a83c3a486475d60eb4a569bb6afbf306ad.tar.gz";
    sha256 = "0s5f7j2akh3g0013880jfbigdaac1z76r9dv46yw6k254ba2r6nq";
  };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          oldOverrides = old.overrides or (_: _: {});

          sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
            grace = ./.;
          };

        in
          pkgsNew.lib.composeExtensions oldOverrides sourceOverrides;
    });
  };

  pkgs = import nixpkgs { config = { }; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskellPackages) grace; }
