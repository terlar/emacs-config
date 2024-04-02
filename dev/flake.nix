{
  description = "Development environment";

  inputs = {
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    dev-flake = {
      url = "github:terlar/dev-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      imports = [
        inputs.dev-flake.flakeModule
        ./test-home-configuration.nix
      ];

      dev = {
        name = "terlar/emacs-config";
        rootSrc = ../.;
      };

      perSystem = {
        config,
        pkgs,
        rootFlake',
        ...
      }: {
        inherit (rootFlake') formatter;

        pre-commit.settings.settings = {
          alejandra.exclude = ["lock"];
          statix.ignore = ["lock"];
        };

        devshells.default = {
          commands = [
            {package = pkgs.gdb;}
            {
              name = "test-emacs-config";
              help = "launch bundled Emacs with configuration from source";
              category = "emacs";
              command = ''
                exec nix run $PRJ_ROOT
              '';
            }
            {
              package = config.packages.reloadEmacsConfig;
              help = "reload and launch Emacs service";
              category = "emacs";
            }
            {
              package = config.packages.updateCaches;
              help = "update Nix caches";
              category = "nix";
            }
            {
              name = "update-screenshots";
              help = "generate new screenshots";
              category = "emacs";
              command = ''
                exec nix run $PRJ_ROOT/dev#updateScreenshots
              '';
            }
          ];
        };

        packages = {
          reloadEmacsConfig = pkgs.writeShellApplication {
            name = "reload-emacs-config";
            text = ''
              systemctl --user restart emacs.service
              while ! emacsclient -a false -e t 2>/dev/null
              do sleep 1; done
              emacsclient -nc
            '';
          };

          updateCaches = pkgs.writeShellApplication {
            name = "update-caches";
            runtimeInputs = [pkgs.cachix];
            text = ''
              cachix use -O . nix-community
              cachix use -O . terlar
            '';
          };

          updateScreenshots = pkgs.writeShellApplication {
            name = "update-screenshots";
            runtimeInputs = [
              pkgs.xorg.lndir
              rootFlake'.packages.emacs-env
            ];
            text = ''
              EMACS_DIR="$(mktemp -td emacs.XXXXXXXXXX)"
              lndir -silent ${rootFlake'.packages.emacs-config} "$EMACS_DIR"
              emacs -fs --init-directory "$EMACS_DIR" --load ${./screenshots.el} --eval '(kill-emacs)'
            '';
          };
        };
      };
    };
}
