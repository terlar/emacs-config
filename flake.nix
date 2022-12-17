{
  description = "Emacs config of Terje";

  nixConfig = {
    extra-substituters = "https://terlar.cachix.org";
    extra-trusted-public-keys = "terlar.cachix.org-1:M8CXTOaJib7CP/jEfpNJAyrgW4qECnOUI02q7cnmh8U=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    devshell.url = "github:numtide/devshell";

    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    twist.url = "github:emacs-twist/twist.nix";
    org-babel.url = "github:emacs-twist/org-babel";

    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };
    nongnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/nongnu.git?ref=main";
      flake = false;
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-parts,
    devshell,
    emacs-overlay,
    home-manager,
    twist,
    org-babel,
    melpa,
    gnu-elpa,
    nongnu-elpa,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} {
      systems = ["x86_64-linux"];

      imports = [
        ./nix/flake/development.nix
        ./nix/flake/test-home-configuration.nix
      ];

      flake = {
        overlays.default = nixpkgs.lib.composeManyExtensions [
          emacs-overlay.overlays.emacs
          org-babel.overlays.default
          twist.overlays.default
          (final: prev: let
            emacs = final.emacsPgtk;
          in {
            emacsEnv =
              (final.emacsTwist {
                emacsPackage = emacs;

                initFiles = [(final.tangleOrgBabelFile "init.el" ./init.org {})];

                lockDir = ./lock;
                inventories = import ./nix/inventories.nix {
                  inherit self;
                  emacsSrc = emacs.src;
                };
                inputOverrides = import ./nix/inputOverrides.nix {inherit (nixpkgs) lib;};
              })
              .overrideScope' (tfinal: tprev: {
                elispPackages = tprev.elispPackages.overrideScope' (
                  prev.callPackage ./nix/packageOverrides.nix {inherit (tprev) emacs;}
                );
              });

            emacsConfig = prev.callPackage self {
              trivialBuild = final.callPackage "${nixpkgs}/pkgs/build-support/emacs/trivial.nix" {
                emacs = (x: x // {inherit (x.emacs) meta nativeComp;}) final.emacsEnv;
              };
            };
          })
        ];
        homeManagerModules = {emacsConfig = import ./nix/home-manager.nix;};
      };

      perSystem = {
        config,
        pkgs,
        ...
      }: {
        legacyPackages = pkgs.extend self.overlays.default;

        packages = {
          inherit (config.legacyPackages) emacsConfig emacsEnv;
          default = config.legacyPackages.emacsConfig;
        };

        checks.build-config = config.packages.emacsConfig;
        checks.build-env = config.packages.emacsEnv;

        apps = config.legacyPackages.emacsEnv.makeApps {
          lockDirName = "lock";
        };
      };
    };
}
