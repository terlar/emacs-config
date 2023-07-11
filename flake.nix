{
  description = "Emacs config of Terje";

  nixConfig = {
    extra-substituters = "https://terlar.cachix.org";
    extra-trusted-public-keys = "terlar.cachix.org-1:M8CXTOaJib7CP/jEfpNJAyrgW4qECnOUI02q7cnmh8U=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      flake = {
        overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
          inputs.emacs-overlay.overlays.emacs
          inputs.org-babel.overlays.default
          inputs.twist.overlays.default
          (final: prev: let
            emacs = final.emacs-pgtk;
          in {
            emacsEnv =
              (final.emacsTwist {
                emacsPackage = emacs;

                initFiles = [(final.tangleOrgBabelFile "init.el" ./init.org {})];

                lockDir = ./lock;
                inventories = import ./nix/inventories.nix {
                  inherit (inputs) self;
                  emacsSrc = emacs.src;
                };
                inputOverrides = import ./nix/inputOverrides.nix {inherit (inputs.nixpkgs) lib;};
              })
              .overrideScope' (_tfinal: tprev: {
                elispPackages = tprev.elispPackages.overrideScope' (
                  prev.callPackage ./nix/packageOverrides.nix {inherit (tprev) emacs;}
                );
              });

            emacsConfig = prev.callPackage inputs.self {
              trivialBuild = final.callPackage "${inputs.nixpkgs}/pkgs/build-support/emacs/trivial.nix" {
                emacs = (x: x // {inherit (x.emacs) meta nativeComp withNativeCompilation;}) final.emacsEnv;
              };
            };
          })
        ];
        homeManagerModules = {emacsConfig = import ./nix/home-manager.nix;};
      };

      perSystem = {
        config,
        pkgs,
        inputs',
        ...
      }: {
        _module.args.pkgs = inputs'.nixpkgs.legacyPackages.extend inputs.self.overlays.default;

        formatter = pkgs.alejandra;

        packages = {
          inherit (pkgs) emacsConfig emacsEnv;
          default = pkgs.emacsConfig;
        };

        checks.build-config = config.packages.emacsConfig;
        checks.build-env = config.packages.emacsEnv;

        apps = pkgs.emacsEnv.makeApps {
          lockDirName = "lock";
        };
      };
    };
}
