{
  description = "Emacs config of Terje";

  nixConfig = {
    extra-substituters = "https://terlar.cachix.org";
    extra-trusted-public-keys = "terlar.cachix.org-1:M8CXTOaJib7CP/jEfpNJAyrgW4qECnOUI02q7cnmh8U=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
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

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      flake = {
        overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
          inputs.emacs-overlay.overlays.emacs
          inputs.org-babel.overlays.default
          inputs.twist.overlays.default

          (
            final: prev:
            let
              emacsPackage = final.emacs-pgtk;
            in
            {
              emacs-env =
                (final.emacsTwist {
                  inherit emacsPackage;

                  initFiles = [ (final.tangleOrgBabelFile "init.el" ./init.org { }) ];

                  lockDir = ./lock;
                  registries = import ./nix/registries.nix {
                    inherit inputs;
                    emacsSrc = emacsPackage.src;
                  };
                  inputOverrides = import ./nix/inputOverrides.nix { inherit (inputs.nixpkgs) lib; };
                }).overrideScope
                  (
                    _: tprev: {
                      elispPackages = tprev.elispPackages.overrideScope (
                        prev.callPackage ./nix/packageOverrides.nix { inherit (tprev) emacs; }
                      );
                    }
                  );

              emacs-config = prev.callPackage inputs.self {
                trivialBuild = final.callPackage "${inputs.nixpkgs}/pkgs/build-support/emacs/trivial.nix" {
                  emacs = final.emacs-env.overrideScope (
                    _: tprev: { inherit (tprev.emacs) meta nativeComp withNativeCompilation; }
                  );
                };
              };
            }
          )
        ];
        homeManagerModules = {
          emacsConfig = import ./nix/home-manager.nix;
        };
      };

      perSystem =
        {
          config,
          pkgs,
          inputs',
          ...
        }:
        {
          _module.args.pkgs = inputs'.nixpkgs.legacyPackages.extend inputs.self.overlays.default;

          packages = {
            inherit (pkgs) emacs-config emacs-env;

            default = pkgs.writeShellApplication {
              name = "test-emacs-config";
              runtimeInputs = [
                pkgs.emacs-env
                pkgs.xorg.lndir
              ];
              text = ''
                XDG_DATA_DIRS="$XDG_DATA_DIRS:${
                  builtins.concatStringsSep ":" (map (x: "${x}/share") pkgs.emacs-config.buildInputs)
                }"
                EMACS_DIR="$(mktemp -td emacs.XXXXXXXXXX)"
                lndir -silent ${pkgs.emacs-config} "$EMACS_DIR"
                emacs --init-directory "$EMACS_DIR" "$@"
              '';
            };
          };

          checks = {
            build-config = config.packages.emacs-config;
            build-env = config.packages.emacs-env;
          };

          apps = pkgs.emacs-env.makeApps { lockDirName = "lock"; };
        };
    };
}
