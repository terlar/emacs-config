{
  description = "Emacs config of Terje";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager = {
      url = "github:rycee/home-manager/bqv-flakes";
      inputs.nixpkgs.follows = "/nixpkgs";
    };
  };

  outputs = { home-manager, emacs-overlay, nixpkgs, utils, self }:
    with nixpkgs;

    {
      overlay = final: prev:
        let
          overrides = final.callPackage ./overrides.nix { };
          extraPackages = import ./packages.nix;
        in (emacs-overlay.overlay final prev) // rec {
          emacsPackages =
            (final.emacsPackagesFor final.emacsGit).overrideScope' overrides;

          emacsEnv = emacsPackages.emacsWithPackages extraPackages;

          emacsConfig = final.callPackage ./. {
            # Ensure build includes packages.
            trivialBuild =
              emacsPackages.trivialBuild.override { emacs = emacsEnv; };
          } // lib.mkIf (self ? lastModifiedDate) {
            version = lib.substring 0 8 self.lastModifiedDate;
          };

          emacsUtils = final.callPackage ./utils.nix { };
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
            ln -s $PWD $XDG_CONFIG_HOME/emacs
            ${pkgs.emacsEnv}/bin/emacs "$@"
          '';

          testEmacsConfig = pkgs.writeShellScriptBin "test-emacs-config" ''
            set -euo pipefail
            export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
            ln -s ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
            ${pkgs.emacsEnv}/bin/emacs "$@"
          '';
        in pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            git
            nixfmt

            devEmacsConfig
            reloadEmacsConfig
            testEmacsConfig
          ];
        };
      });
}
