{
  description = "Emacs config of Terje";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
  };

  outputs = { home-manager, emacs-overlay, nixpkgs, utils, self, ... }:
    with nixpkgs;

    {
      overlay = final: prev:
        let override = prev.callPackage ./overrides.nix { };
        in (emacs-overlay.overlay final prev) // rec {
          emacsEnv = final.emacsWithPackagesFromUsePackage {
            config = ./init.org;
            package = final.emacsPgtk;

            extraEmacsPackages = epkgs: [ epkgs.org-plus-contrib ];

            inherit override;
          };

          emacsConfig = prev.callPackage ./. ({
            # Ensure build includes packages.
            trivialBuild =
              prev.emacsPackages.trivialBuild.override { emacs = emacsEnv; };
          } // lib.optionalAttrs (self ? lastModifiedDate) {
            version = lib.substring 0 8 self.lastModifiedDate;
          });

          emacsUtils = prev.callPackage ./utils.nix { };
        };

      homeManagerModules = { emacsConfig = import ./home-manager.nix; };
    } // utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in {
        packages = { inherit (pkgs) emacsConfig emacsEnv emacsUtils; };
        defaultPackage = self.packages.${system}.emacsConfig;

        devShell = let
          reloadEmacsConfig = pkgs.writeShellScriptBin "reload-emacs-config" ''
            set -euo pipefail
            systemctl --user restart emacs.service
            while ! emacsclient -a false -e t 2>/dev/null
            do sleep 1; done
            emacsclient -nc
          '';

          devEmacsConfig = pkgs.writeShellScriptBin "dev-emacs-config" ''
            set -euo pipefail
            export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
            mkdir -p $XDG_CONFIG_HOME/emacs
            ${pkgs.xorg.lndir}/bin/lndir -silent $PWD $XDG_CONFIG_HOME/emacs
            ${pkgs.emacsEnv}/bin/emacs "$@"
          '';

          testEmacsConfig = pkgs.writeShellScriptBin "test-emacs-config" ''
            set -euo pipefail
            export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
            mkdir -p $XDG_CONFIG_HOME/emacs
            ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
            ${pkgs.emacsEnv}/bin/emacs "$@"
          '';

          updateCaches = pkgs.writeShellScriptBin "update-caches" ''
            ${pkgs.cachix}/bin/cachix use -O . nix-community
            ${pkgs.cachix}/bin/cachix use -O . terlar
          '';
        in pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            gdb
            git
            nixUnstable
            nixfmt

            devEmacsConfig
            reloadEmacsConfig
            testEmacsConfig
            updateCaches
          ];
        };
      });
}
