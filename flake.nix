{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/22.05;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, self }:
    let
      overlay = compiler: self: super: {
        codemirror = self.fetchzip {
          url = "https://codemirror.net/5/codemirror.zip";
          sha256 = "sha256-TS4JVTRIwdWj/CihELhhC3Rte9DT0Tv239eZKR6MT6w=";
        };

        haskell = super.haskell // {
          packages = super.haskell.packages // {
            "${compiler}" = super.haskell.packages."${compiler}".override (old: {
              overrides =
                let
                  hlib = self.haskell.lib;

                  mass = f:
                    self.lib.fold
                      (name:
                        self.lib.composeExtensions
                          (hself: hsuper: {
                            "${name}" = f hsuper."${name}";
                          })
                      )
                      (_: _: { });

                  manualOverrides = hself: hsuper: {
                    grace =
                      hlib.justStaticExecutables
                        (hlib.overrideCabal hsuper.grace (old: {
                          doCheck = false;

                          src =
                            self.lib.cleanSourceWith
                              { inherit (old) src;

                                filter = path: type:
                                      self.lib.cleanSourceFilter path type
                                  &&  ! (  (   type == "regular"
                                           &&  (   self.lib.hasSuffix ".nix" (baseNameOf path)
                                               ||  self.lib.hasSuffix ".md" (baseNameOf path)
                                               ||  baseNameOf path == "cabal.project.local"
                                               )
                                           )
                                        || (   type == "directory"
                                           &&  (builtins.elem (baseNameOf path) [
                                                 "dist"
                                                 "dist-newstyle"
                                                 "examples"
                                                 "prelude"
                                                 "website"
                                               ])
                                           )
                                        );
                              };
                        }));
                  };

                  ghcjsOverrides =
                    self.lib.fold self.lib.composeExtensions (_: _: { }) [
                      (mass hlib.dontCheck [
                        "asn1-encoding"
                        "bsb-http-chunked"
                        "commonmark"
                        "conduit"
                        "cryptonite"
                        "foldl"
                        "ghcjs-fetch"
                        "hedgehog"
                        "http-date"
                        "hourglass"
                        "insert-ordered-containers"
                        "iproute"
                        "memory"
                        "mono-traversable"
                        "network-byte-order"
                        "prettyprinter-ansi-terminal"
                        "servant-client"
                        "streaming-commons"
                        "text-short"
                        "unix-time"
                        "vector"
                        "x509"
                        "x509-store"
                        "yaml"
                        "zlib"
                      ])

                      (mass hlib.dontHaddock [
                        "grace"
                        "openai"
                      ])

                      (mass hlib.doJailbreak [
                        "ghcjs-fetch"
                        "openai"
                      ])

                      (hself: hsuper: {
                        ghcjs-fetch =
                          hlib.addBuildDepends
                            hsuper.ghcjs-fetch
                            [ hself.ghcjs-base ];

                        aeson = hself.aeson_1_5_6_0;

                        entropy =
                          hlib.addBuildDepends
                            hsuper.entropy
                            [ hself.ghcjs-dom
                              hself.jsaddle
                            ];

                        haskeline = hself.haskeline_0_8_2;

                        network = hsuper.network.overrideAttrs (old: {
                          dontUpdateAutotoolsGnuConfigScripts = true;
                        });

                        servant-multipart-client =
                          hsuper.servant-multipart-client.override (old: {
                            servant-multipart = null;

                            servant-server = null;

                            warp = null;
                          });

                      })
                    ];

                  sourceOverrides = hlib.packageSourceOverrides {
                    grace = ./.;
                  };

                  directoryOverrides = hlib.packagesFromDirectory {
                    directory = ./dependencies;
                  };

                  oldOverrides = old.overrides or (_: _: {});

                in
                  self.lib.fold self.lib.composeExtensions oldOverrides
                    (   [ sourceOverrides
                          directoryOverrides
                          manualOverrides
                        ]
                    ++  self.lib.optional (compiler == "ghcjs") ghcjsOverrides
                    );
            });
          };
        };

        haskell-language-server =
          super.haskell-language-server.override (old: {
            haskellPackages = super.haskell.packages."${compiler}";

            supportedGhcVersions = [ "902" ];
          });

        website = self.runCommand "try-grace" { nativeBuildInputs = [ self.rsync ]; } ''
          mkdir -p $out/{css,js,prelude,prompts,examples}
          rsync --recursive ${./website}/ $out
          rsync --recursive ${./prelude}/ $out/prelude
          rsync --recursive ${./prompts}/ $out/prompts
          rsync --recursive ${./examples}/ $out/examples
          ln --symbolic ${self.codemirror}/lib/codemirror.css --target-directory=$out/css
          ln --symbolic ${self.codemirror}/lib/codemirror.js --target-directory=$out/js
          ln --symbolic ${self.codemirror}/mode/python/python.js --target-directory=$out/js
          ln --symbolic ${self.haskell.packages."${compiler}".grace}/bin/try-grace.jsexe/all.js --target-directory=$out/js
        '';
      };

   in
      utils.lib.eachDefaultSystem (system:
        let
          withCompiler = compiler:
            let
              config.allowBroken = true;

              pkgs = import nixpkgs {
                inherit config system;

                overlays = [ (overlay compiler) ];
              };

              grace = pkgs.haskell.packages."${compiler}".grace;

              website = pkgs.website;

              shell = pkgs.haskell.packages."${compiler}".shellFor {
                packages = hpkgs: [ hpkgs.grace ];

                nativeBuildInputs = [ pkgs.haskell-language-server ];

                withHoogle = true;

                doBenchmark = true;
              };

            in
              { inherit grace shell website; };

          ghc = withCompiler "ghc902";

          ghcjs = withCompiler "ghcjs";

        in
          { packages = {
              default = ghc.grace;

              website = ghcjs.website;
            };

            apps.default = {
              type = "app";

              program = nixpkgs.lib.getExe self.packages."${system}".default;
            };

            devShells = {
              default = ghc.shell;

              ghcjs = ghcjs.grace.env;
            };
          }) // {
            overlays = nixpkgs.lib.genAttrs [ "ghc902" "ghcjs" ] overlay;
          };

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
