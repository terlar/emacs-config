{
  description = "Emacs config of Terje";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { home-manager, emacs-overlay, nixpkgs, self, ... }:
    let
      inherit (nixpkgs) lib;

      systems = [ "x86_64-linux" ];
      forAllSystems = f: lib.genAttrs systems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = final: prev:
        let override = prev.callPackage ./overrides.nix { };
        in
        (emacs-overlay.overlay final prev) // rec {
          emacsEnv = final.emacsWithPackagesFromUsePackage {
            package = final.emacsPgtkGcc;

            config = ./init.org;
            alwaysEnsure = false;
            inherit override;
          };

          emacsConfig = prev.callPackage ./. ({
            # Ensure build includes packages.
            trivialBuild =
              prev.emacsPackages.trivialBuild.override { emacs = emacsEnv; };
          } // lib.optionalAttrs (self ? lastModifiedDate) {
            version = lib.substring 0 8 self.lastModifiedDate;
          });
        };

      packages = forAllSystems (system: { inherit (nixpkgsFor.${system}) emacsConfig emacsEnv; });
      defaultPackage = forAllSystems (system: self.packages.${system}.emacsConfig);

      homeManagerModules = { emacsConfig = import ./home-manager.nix; };
      homeConfigurations = forAllSystems (system: home-manager.lib.homeManagerConfiguration {
        inherit system;
        pkgs = nixpkgsFor.${system};
        username = "test";
        homeDirectory = "/home/test";
        extraModules = [ self.homeManagerModules.emacsConfig ];
        configuration = {
          custom.emacsConfig = {
            enable = true;
            erc = nixpkgsFor.${system}.writeText "ercrc.el" ''
              ;; Testing testing
            '';
          };
        };
      });

      checks = forAllSystems (system: {
        build-home-configuration =
          self.homeConfigurations.${system}.activationPackage;
      });

      devShell = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};

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
              ln -s $HOME/.config/fontconfig $XDG_CONFIG_HOME/.
              ${pkgs.emacsEnv}/bin/emacs "$@"
            '';

            testEmacsConfig = pkgs.writeShellScriptBin "test-emacs-config" ''
              set -euo pipefail
              export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
              mkdir -p $XDG_CONFIG_HOME/emacs
              ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
              ln -s $HOME/.config/fontconfig $XDG_CONFIG_HOME/.
              ${pkgs.emacsEnv}/bin/emacs "$@"
            '';

            updateCaches = pkgs.writeShellScriptBin "update-caches" ''
              ${pkgs.cachix}/bin/cachix use -O . nix-community
              ${pkgs.cachix}/bin/cachix use -O . terlar
            '';

            updateScreenshots = pkgs.writeShellScriptBin "update-screenshots" ''
              set -euo pipefail
              export XDG_CONFIG_HOME=$(mktemp -td xdg-config.XXXXXXXXXX)
              mkdir -p $XDG_CONFIG_HOME/emacs
              ${pkgs.xorg.lndir}/bin/lndir -silent ${pkgs.emacsConfig} $XDG_CONFIG_HOME/emacs
              ln -s $HOME/.config/fontconfig $XDG_CONFIG_HOME/.
              ${pkgs.emacsEnv}/bin/emacs -fs --load ${./screenshots.el} --eval '(kill-emacs)'
            '';
          in
          pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              gdb
              git
              nix_2_4
              nixpkgs-fmt

              devEmacsConfig
              reloadEmacsConfig
              testEmacsConfig
              updateCaches
              updateScreenshots
            ];
          });
    };
}
