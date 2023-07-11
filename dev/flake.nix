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
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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
        # inherit (rootFlake') formatter;

        pre-commit.settings.settings = {
          alejandra.exclude = ["lock"];
          statix.ignore = ["lock"];
        };

        devshells.default = {
          commands = [
            {package = pkgs.gdb;}
            {
              package = config.packages.devEmacsConfig;
              help = "launch bundled Emacs with configuration from source";
              category = "emacs";
            }
            {
              package = config.packages.reloadEmacsConfig;
              help = "reload and launch Emacs service";
              category = "emacs";
            }
            {
              package = config.packages.testEmacsConfig;
              help = "launch bundled Emacs with fixed configuration";
              category = "emacs";
            }
            {
              package = config.packages.updateCaches;
              help = "update Nix caches";
              category = "nix";
            }
            {
              package = config.packages.updateScreenshots;
              help = "generate new screenshots";
              category = "emacs";
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

          devEmacsConfig = pkgs.writeShellApplication {
            name = "dev-emacs-config";
            runtimeInputs = [
              pkgs.xorg.lndir
              rootFlake'.packages.emacsEnv
            ];
            text = ''
              XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
              export XDG_CONFIG_HOME
              mkdir -p "$XDG_CONFIG_HOME/emacs"
              lndir -silent "$PWD" "$XDG_CONFIG_HOME/emacs"
              ln -s "$HOME/.config/fontconfig" "$XDG_CONFIG_HOME/."
              emacs "$@"
            '';
          };

          testEmacsConfig = pkgs.writeShellApplication {
            name = "test-emacs-config";
            runtimeInputs = [
              pkgs.xorg.lndir
              rootFlake'.packages.emacsEnv
            ];
            text = ''
              XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
              export XDG_CONFIG_HOME
              mkdir -p "$XDG_CONFIG_HOME/emacs"
              lndir -silent ${rootFlake'.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
              ln -s "$HOME/.config/fontconfig" "$XDG_CONFIG_HOME/."
              emacs "$@"
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
              rootFlake'.packages.emacsEnv
            ];
            text = ''
              XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
              export XDG_CONFIG_HOME
              mkdir -p "$XDG_CONFIG_HOME/emacs"
              lndir -silent ${rootFlake'.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
              ln -s "$HOME/.config/fontconfig" "$XDG_CONFIG_HOME/."
              emacs -fs --load ${./screenshots.el} --eval '(kill-emacs)'
            '';
          };
        };
      };
    };
}
