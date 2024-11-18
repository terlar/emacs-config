{
  description = "Emacs config of Terje";

  nixConfig = {
    extra-substituters = "https://terlar.cachix.org";
    extra-trusted-public-keys = "terlar.cachix.org-1:M8CXTOaJib7CP/jEfpNJAyrgW4qECnOUI02q7cnmh8U=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

    flake-parts.url = "github:hercules-ci/flake-parts";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

    twist.url = "github:emacs-twist/twist.nix";
    org-babel.url = "github:emacs-twist/org-babel";

    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main&shallow=0";
      flake = false;
    };
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    nongnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main&shallow=0";
      flake = false;
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        inputs.flake-parts.flakeModules.partitions
      ];

      partitionedAttrs = {
        checks = "dev";
        devShells = "dev";
        packages = "dev";
      };

      partitions.dev = {
        extraInputsFlake = ./dev;
        module.imports = [ ./dev/flake-module.nix ];
      };

      flake = {
        homeManagerModules.emacsConfig = import ./nix/home-manager.nix;
      };

      perSystem =
        {
          config,
          pkgs,
          inputs',
          ...
        }:
        {
          packages =
            let
              inherit (inputs.nixpkgs) lib;

              initEl = lib.pipe ./init.org [
                builtins.readFile
                (inputs.org-babel.lib.tangleOrgBabel { })
                (builtins.toFile "init.el")
              ];

              packageOverrides = _: prev: {
                elispPackages = prev.elispPackages.overrideScope (
                  pkgs.callPackage ./nix/packageOverrides.nix { inherit (prev) emacs; }
                );
              };
            in
            {
              emacs = inputs'.emacs-overlay.packages.emacs-git;
              emacs-env =
                (inputs.twist.lib.makeEnv {
                  inherit pkgs;
                  emacsPackage = config.packages.emacs;

                  initFiles = [ initEl ];
                  lockDir = ./lock;

                  registries = import ./nix/registries.nix {
                    inherit inputs;
                    emacsSrc = config.packages.emacs.src;
                  };

                  inputOverrides = import ./nix/inputOverrides.nix { inherit lib; };
                }).overrideScope
                  packageOverrides;

              emacs-config = pkgs.callPackage inputs.self {
                buildElispPackage = (inputs.twist.lib.buildElispPackage pkgs).override {
                  inherit (config.packages) emacs;
                };

                elispInputs = lib.pipe config.packages.emacs-env.elispPackages [
                  builtins.attrValues
                  (builtins.filter lib.isDerivation)
                ];
              };
            };

          apps = config.packages.emacs-env.makeApps { lockDirName = "lock"; };
        };
    };
}
