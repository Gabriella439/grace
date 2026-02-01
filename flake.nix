{
  inputs = {
    garnix-lib.url = "github:garnix-io/garnix-lib";

    nixpkgs.url = github:NixOS/nixpkgs/24.11;

    utils.url = github:numtide/flake-utils;
  };

  outputs = { garnix-lib, nixpkgs, utils, self }:
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

                  oldOverrides = old.overrides or (_: _: {});

                  directoryOverrides = hlib.packagesFromDirectory {
                    directory = ./dependencies;
                  };

                  ghcjsManualOverrides =
                    self.lib.fold self.lib.composeExtensions (_: _: { }) [
                      (mass hlib.doJailbreak [
                        "aeson_1_5_6_0"
                        "ghcjs-base"
                        "ghcjs-fetch"
                        "openai"
                      ])

                      (hself: hsuper: {
                        mkDerivation = args: hsuper.mkDerivation (args // {
                          doCheck = false;
                          doHaddock = false;
                        });

                        aeson = hself.aeson_1_5_6_0;

                        entropy =
                          hlib.addBuildDepends
                            hsuper.entropy
                            [ hself.ghcjs-dom
                              hself.jsaddle
                            ];

                        exceptions = hself.exceptions_0_10_8;

                        grace = hself.grace-ghcjs;

                        ghcjs-fetch =
                          hlib.addBuildDepends
                            hsuper.ghcjs-fetch
                            [ hself.ghcjs-base ];

                        # haskeline = hself.haskeline_0_8_2;

                        network = hsuper.network.overrideAttrs (old: {
                          dontUpdateAutotoolsGnuConfigScripts = true;
                        });

                        servant-multipart-client =
                          hlib.overrideCabal
                            (hsuper.servant-multipart-client.override (old: {
                              servant-multipart = null;

                              servant-server = null;

                              warp = null;
                            }))
                            (old: {
                              buildDepends = (old.buildDepends or []) ++ [
                                hself.servant
                                hself.servant-client
                              ];
                              postPatch = (old.postPatch or "") +
                                ''
                                sed -i -e 's/servant .*<0.19/servant/' -e 's/servant-client-core .*<0.19/servant-client-core/' servant-multipart-client.cabal
                                '';
                            });
                      })
                    ];

                  ghcjsSourceOverrides = hlib.packageSourceOverrides {
                    modern-uri = "0.3.4.4";

                    unordered-containers = "0.2.18.0";
                  };

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
                                               ||  self.lib.hasSuffix ".yaml" (baseNameOf path)
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

                    openai = hlib.dontCheck hsuper.openai;
                  };

                in
                  self.lib.fold self.lib.composeExtensions oldOverrides
                    (   [ directoryOverrides ]
                    ++  self.lib.optional (compiler == "ghcjs") ghcjsSourceOverrides
                    ++  self.lib.optional (compiler == "ghcjs") ghcjsManualOverrides
                    ++  [ manualOverrides ]
                    );
            });
          };
        };

        haskell-language-server =
          super.haskell-language-server.override (old: {
            haskellPackages = super.haskell.packages."${compiler}";

            supportedGhcVersions = [ "96" ];
          });

        docker-stream =
          let
            configuration = self.writeText "nginx.conf"
              ''
              events {
              }

              error_log  /dev/stderr;

              http {
                access_log /dev/stdout;

                include ${self.nginx}/conf/mime.types;

                default_type  application/octet-stream;

                server {
                  listen 8080;

                  root ${self.website};

                  index index.html;

                  add_header X-Content-Type-Options nosniff;
                  add_header X-Frame-Options SAMEORIGIN;

                  gzip on;
                  gzip_types application/javascript;

                  location ~ \.js$ {
                    add_header Cache-Control "public, max-age=31536000, immutable";
                  }
                }
              }

              daemon off;
              '';

            args = {
              name = "grace";

              tag = "latest";

              config = {
                Cmd = [ (self.lib.getExe self.nginx) "-c" configuration ];

                User = "65534:65534";
              };

              enableFakechroot = true;

              fakeRootCommands =
                ''
                paths=(
                  /var/cache/nginx/{client_body,proxy,fastcgi,uwsgi,scgi}
                  /var/log/nginx
                )

                mkdir -p "''${paths[@]}"

                chown -R 65534:65534 "''${paths[@]}"
                '';
            };

          in
            self.dockerTools.streamLayeredImage (args // {
              passthru = { inherit args; };
            });

        docker-image =
          self.dockerTools.buildLayeredImage self.docker-stream.passthru.args;

        website = self.runCommand "try-grace" { nativeBuildInputs = [ self.rsync ]; } ''
          js=js/''${out:11:32}

          mkdir -p $out/{css,prelude,prompts,examples} $out/$js

          rsync --recursive ${./website}/ $out
          rsync --recursive ${./prelude}/ $out/prelude
          rsync --recursive ${./prompts}/ $out/prompts
          rsync --recursive ${./examples}/ $out/examples

          cp ${self.codemirror}/lib/codemirror.css --target-directory=$out/css
          cp ${self.codemirror}/lib/codemirror.js --target-directory=$out/$js
          cp ${self.codemirror}/addon/display/placeholder.js --target-directory=$out/$js
          cp ${self.codemirror}/mode/python/python.js --target-directory=$out/$js
          cp ${self.haskell.packages."${compiler}".grace}/bin/try-grace.jsexe/all.js --target-directory=$out/$js

          sed --in-place 's!src="js/!src="'"$js"'/!g' $out/index.html
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

              shell = pkgs.haskell.packages."${compiler}".shellFor {
                packages = hpkgs: [ (pkgs.haskell.lib.doCheck hpkgs.grace) ];

                nativeBuildInputs = [ pkgs.haskell-language-server ];

                withHoogle = true;

                doBenchmark = true;
              };

            in
              { inherit grace shell;

                inherit (pkgs) docker-image docker-stream website;
              };

          ghc = withCompiler "ghc96";

          ghcjs = withCompiler "ghcjs";

        in
          { packages = {
              default = ghc.grace;

              docker-image = ghcjs.docker-image;

              docker-stream = ghcjs.docker-stream;

              website = ghcjs.website;
            };

            apps = {
              default = {
                type = "app";

                program = nixpkgs.lib.getExe self.packages."${system}".default;
              };

              docker-stream = {
                type = "app";

                program = "${self.packages."${system}".docker-stream}";
              };
            };

            devShells = {
              default = ghc.shell;

              ghcjs = ghcjs.grace.env;
            };
          }) // {
            overlays = nixpkgs.lib.genAttrs [ "ghc96" "ghcjs" ] overlay;

            nixosConfigurations =
              let
                defaultModule = { pkgs, ... }: {
                  documentation.nixos.enable = false;

                  networking = {
                    firewall.allowedTCPPorts = [ 80 443 ];

                    hostName = "trygrace";
                  };

                  nix.settings.trusted-users = [
                    "gabriella"
                  ];

                  nixpkgs = {
                    config.allowBroken = true;

                    overlays = [ self.overlays.ghcjs ];
                  };

                  security = {
                    acme = {
                      acceptTerms = true;

                      email = "GenuineGabriella@gmail.com";
                    };

                    sudo.wheelNeedsPassword = false;
                  };

                  services = {
                    nginx = {
                      enable = true;

                      recommendedGzipSettings = true;

                      recommendedOptimisation = true;

                      recommendedTlsSettings = true;

                      recommendedProxySettings = true;

                      virtualHosts = {
                        "trygrace.dev" = {
                          default = true;

                          forceSSL = true;

                          enableACME = true;

                          locations."/" = {
                            index = "index.html";

                            root = pkgs.website;
                          };
                        };
                      };
                    };

                    openssh.enable = true;
                  };

                  system.stateVersion = "22.05";

                  time.timeZone = "America/Los_Angeles";

                  users = {
                    mutableUsers = false;

                    users.gabriella = {
                      isNormalUser = true;

                      extraGroups = [ "wheel" ];

                      openssh.authorizedKeys.keys = [
                        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMuH6HDuoMlK7b2Ovm5VKt9P3aRrJ2HeUPptKG+21kjL gabriella@Gabriellas-MacBook-Pro.local"
                        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/nXKUEQsKukm+eIKmwzWoybKzwQTiYzGZqrWmHYwYe gabriella@lucina.local"
                      ];
                    };
                  };
                };

                qemuModule = { modulesPath, ... }: {
                  imports = [
                    "${modulesPath}/virtualisation/qemu-vm.nix"
                  ];

                  config.virtualisation.host.pkgs = import nixpkgs {
                    system = "aarch64-darwin";

                    config.allowBroken = true;

                    overlays = [ self.overlays.ghcjs ];
                  };
                };

                garnixModule = {
                  imports = [ garnix-lib.nixosModules.garnix ];

                  config.garnix.server = {
                    enable = true;

                    persistence = {
                      enable = true;

                      name = "main";
                    };
                  };
                };

                garnix = nixpkgs.lib.nixosSystem {
                  system = "x86_64-linux";

                  modules = [ defaultModule garnixModule ];
                };

                qemu = nixpkgs.lib.nixosSystem {
                  system = "x86_64-linux";

                  modules = [ defaultModule qemuModule ];
                };

            in
              { default = garnix;

                inherit garnix qemu;
              };
          };

  nixConfig = {
    extra-substituters = [ "https://cache.garnix.io" ];

    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };
}
