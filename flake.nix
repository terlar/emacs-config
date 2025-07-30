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

    melpa = {
      url = "github:melpa/melpa";
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
        formatter = "dev";
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
          packages = {
            emacs = inputs'.emacs-overlay.packages.emacs-git.overrideAttrs (old: {
              src = pkgs.fetchFromGitHub {
                owner = "emacs-mirror";
                repo = "emacs";
                inherit (old.src) rev;
                sha256 = old.src.outputHash;
              };
            });

            emacs-pgtk = inputs'.emacs-overlay.packages.emacs-git-pgtk.overrideAttrs (old: {
              src = pkgs.fetchFromGitHub {
                owner = "emacs-mirror";
                repo = "emacs";
                inherit (old.src) rev;
                sha256 = old.src.outputHash;
              };
            });

            emacs-env = pkgs.callPackage ./nix/packages/emacs-env {
              org-babel-lib = inputs.org-babel.lib;
              twist-lib = inputs.twist.lib;
              rootPath = ./.;
              melpaSrc = inputs.melpa.outPath;
              inherit (config.packages) emacs;
            };

            emacs-env-pgtk = config.packages.emacs-env.override {
              emacs = config.packages.emacs-pgtk;
            };

            emacs-config = pkgs.callPackage ./nix/packages/emacs-config {
              twist-lib = inputs.twist.lib;
              rootPath = ./.;
              inherit (config.packages) emacs emacs-env;
            };

            emacs-config-pgtk = config.packages.emacs-config.override {
              emacs = config.packages.emacs-pgtk;
              emacs-env = config.packages.emacs-env-pgtk;
            };
          };

          apps = config.packages.emacs-env.makeApps { lockDirName = "lock"; };
        };
    };
}
