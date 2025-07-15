{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let withCompiler = compiler:
            let overlay = pkgsNew: pkgsOld: {
                  codemirror = pkgsNew.fetchzip {
                    url = "https://codemirror.net/5/codemirror.zip";
                    sha256 = "sha256-G8m2Ba+wSkk0u4Ux7dokA3TuP/SIfgN/TLiqUNLW8e0=";
                  };

                  haskell = pkgsOld.haskell // {
                    packages = pkgsOld.haskell.packages // {
                      "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
                        overrides =
                          let
                            oldOverrides = old.overrides or (_: _: {});

                            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                              ghcjs-fetch =
                                pkgsNew.haskell.lib.addBuildDepend
                                  (pkgsNew.haskell.lib.doJailbreak
                                    (pkgsNew.haskell.lib.dontCheck haskellPackagesOld.ghcjs-fetch)
                                  )
                                  [ haskellPackagesNew.ghcjs-base ];

                              grace =
                                pkgsNew.haskell.lib.overrideCabal
                                  haskellPackagesOld.grace
                                  (old: {
                                    doCheck = false;

                                    doHaddock = false;

                                    src =
                                      pkgsNew.lib.cleanSourceWith
                                        { inherit (old) src;

                                          filter = path: type:
                                                pkgsNew.lib.cleanSourceFilter path type
                                            &&  (!((pkgsNew.lib.hasPrefix "result" (baseNameOf path) && type == "symlink")
                                                || (pkgsNew.lib.hasSuffix ".nix" (baseNameOf path) && type == "regular")
                                                || (pkgsNew.lib.hasSuffix ".md" (baseNameOf path) && type == "regular")
                                                || (baseNameOf path == "cabal.project.local" && type == "regular")
                                                || (baseNameOf path == "dist" && type == "directory")
                                                || (baseNameOf path == "dist-newstyle" && type == "directory")
                                                || (baseNameOf path == "examples" && type == "directory")
                                                || (baseNameOf path == "prelude" && type == "directory")
                                                || (baseNameOf path == "website" && type == "directory")
                                                ));
                                        };
                                  });

                              aeson = haskellPackagesNew.aeson_1_5_6_0;

                              asn1-encoding =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.asn1-encoding;

                              bsb-http-chunked =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.bsb-http-chunked;

                              conduit =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.conduit;

                              cryptonite =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.cryptonite;

                              entropy =
                                pkgsNew.haskell.lib.addBuildDepends
                                  haskellPackagesOld.entropy
                                  [ haskellPackagesNew.ghcjs-dom
                                    haskellPackagesNew.jsaddle
                                  ];

                              foldl =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.foldl;

                              haskeline = haskellPackagesNew.haskeline_0_8_2;

                              hedgehog =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.hedgehog;

                              http-date =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.http-date;

                              hourglass =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.hourglass;

                              insert-ordered-containers =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.insert-ordered-containers;

                              iproute =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.iproute;

                              memory =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.memory;

                              mono-traversable =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.mono-traversable;

                              network =
                                  haskellPackagesOld.network.overrideAttrs (old: {
                                    dontUpdateAutotoolsGnuConfigScripts = true;
                                  });

                              network-byte-order =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.network-byte-order;

                              openai =
                                pkgsNew.haskell.lib.doJailbreak
                                  (pkgsNew.haskell.lib.dontHaddock
                                    haskellPackagesOld.openai
                                  );

                              prettyprinter-ansi-terminal =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.prettyprinter-ansi-terminal;

                              servant-client =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.servant-client;

                              servant-multipart-client =
                                haskellPackagesOld.servant-multipart-client.override (old: {
                                  servant-multipart = null;

                                  servant-server = null;

                                  warp = null;
                                });

                              streaming-commons =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.streaming-commons;

                              text-short =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.text-short;

                              unix-time =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.unix-time;

                              vector =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.vector;

                              x509 =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.x509;

                              x509-store =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.x509-store;

                              yaml =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.yaml;

                              zlib =
                                pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.zlib;
                            };

                            sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
                              grace = ./.;
                            };

                            directoryOverrides = pkgsNew.haskell.lib.packagesFromDirectory {
                              directory = ./dependencies;
                            };

                          in
                            pkgsNew.lib.fold pkgsNew.lib.composeExtensions oldOverrides
                              (   [ sourceOverrides
                                    directoryOverrides
                                  ]
                              ++  pkgsNew.lib.optional (compiler == "ghcjs") manualOverrides
                              );
                      });
                    };
                  };

                  website = pkgsNew.runCommand "try-grace" { nativeBuildInputs = [ pkgsNew.rsync ]; } ''
                    mkdir -p $out/{css,js,prelude,prompts,examples}
                    rsync --recursive ${./website}/ $out
                    rsync --recursive ${./prelude}/ $out/prelude
                    rsync --recursive ${./prompts}/ $out/prompts
                    rsync --recursive ${./examples}/ $out/examples
                    ln --symbolic ${pkgsNew.codemirror}/lib/codemirror.css --target-directory=$out/css
                    ln --symbolic ${pkgsNew.codemirror}/lib/codemirror.js --target-directory=$out/js
                    ln --symbolic ${pkgsNew.codemirror}/mode/python/python.js --target-directory=$out/js
                    ln --symbolic ${pkgsNew.haskell.packages."${compiler}".grace}/bin/try-grace.jsexe/all.js --target-directory=$out/js
                  '';
                };
                config.allowBroken = true;
                pkgs = import nixpkgs { inherit config system; overlays = [ overlay ]; };

                grace = pkgs.haskell.packages."${compiler}".grace;

                graceMinimal =
                  pkgs.haskell.lib.justStaticExecutables
                    (grace.overrideAttrs (_: { doCheck = false; }));

                website = pkgs.website;
             in
            { inherit grace graceMinimal website; };

          withDefaultCompiler = withCompiler "ghc902";
          withghcjs = withCompiler "ghcjs";
       in
      rec {
        packages = {
          default = withDefaultCompiler.graceMinimal;
          website = withghcjs.website;
        };

        defaultPackage = packages.default;

        apps.default = {
          type = "app";
          program = "${withDefaultCompiler.graceMinimal}/bin/grace";
        };

        defaultApp = apps.default;

        devShells = {
          default = withDefaultCompiler.grace.env;
          ghcjs = withghcjs.grace.env;
        };

        devShell = devShells.default;
      });

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
