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
          emacs-overlay.overlays.default
          (final: prev: {
            emacsEnv = final.emacsWithPackagesFromUsePackage {
              package = final.emacsPgtkNativeComp;

              config = ./init.org;
              alwaysEnsure = false;
              override = final.callPackage ./nix/overrides.nix {};
            };

            emacsConfig = (prev.emacsPackagesFor final.emacsEnv.emacs).callPackage ./. ({
                packageRequires = final.emacsEnv.explicitRequires;
              }
              // nixpkgs.lib.optionalAttrs (self ? lastModifiedDate) {
                version = nixpkgs.lib.substring 0 8 self.lastModifiedDate;
              });
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
      };
    };
}
