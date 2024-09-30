{ inputs, ... }:

{
  imports = [
    inputs.dev-flake.flakeModule
    ./test-home-configuration.nix
  ];

  dev.name = "terlar/emacs-config";

  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    {
      pre-commit.settings.hooks = {
        statix.settings.ignore = [ "lock" ];
      };

      treefmt = {
        programs.nixfmt = {
          enable = true;
          package = pkgs.nixfmt-rfc-style;
        };
        settings.formatter.nixfmt.excludes = [ "lock/**/*.nix" ];
      };

      devshells.default = {
        commands = [
          { package = pkgs.gdb; }
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
          runtimeInputs = [ pkgs.cachix ];
          text = ''
            cachix use -O . nix-community
            cachix use -O . terlar
          '';
        };

        updateScreenshots = pkgs.writeShellApplication {
          name = "update-screenshots";
          runtimeInputs = [
            pkgs.xorg.lndir
            config.packages.emacs-env
          ];
          text = ''
            EMACS_DIR="$(mktemp -td emacs.XXXXXXXXXX)"
            lndir -silent ${config.packages.emacs-config} "$EMACS_DIR"
            emacs -fs --init-directory "$EMACS_DIR" --load ${./screenshots.el} --eval '(kill-emacs)'
          '';
        };
      };
    };
}
