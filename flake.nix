{ inputs.all-cabal-hashes = {
    url = "github:commercialhaskell/all-cabal-hashes/hackage";

    flake = false;
  };

  outputs = { all-cabal-hashes, flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config.allowUnfree = true;

        pkgs = import nixpkgs {
          inherit config system;

          overlays = [ self.overlays.default ];
        };

      in
        { packages.default = pkgs.haskellPackagesCustom.grace;

          devShells.default = pkgs.haskellPackagesCustom.shellFor {
            packages = hpkgs: [
              hpkgs.grace
            ];

            nativeBuildInputs = [
              pkgs.haskell-language-server

              (pkgs.vscode-with-extensions.override {
                vscodeExtensions = [
                  pkgs.vscode-extensions.haskell.haskell
                  pkgs.vscode-extensions.justusadam.language-haskell
                ];
              })
            ];

            withHoogle = true;

            doBenchmark = true;
          };
        }
    ) // {
      overlays.default = self: super: {
        inherit all-cabal-hashes;

        haskellPackagesCustom = self.haskellPackages.override (old: {
          overrides =
            let
              hlib = self.haskell.lib.compose;
            in
              self.lib.composeManyExtensions [
                (hlib.packageSourceOverrides {
                  grace = ./.;
                })

                (hlib.packagesFromDirectory {
                  directory = ./dependencies;
                })

                (hself: hsuper: {
                })
              ];
        });
      };
    };

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
