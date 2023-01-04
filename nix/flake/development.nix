{inputs, ...}: {
  imports = [inputs.pre-commit-hooks.flakeModule];

  perSystem = {
    config,
    pkgs,
    inputs',
    ...
  }: {
    formatter = pkgs.alejandra;

    pre-commit = {
      check.enable = true;
      settings = {
        hooks.alejandra.enable = true;
        hooks.deadnix.enable = true;

        hooks.statix.enable = true;
        settings.statix.ignore = ["lock"];
      };
    };

    devShells.default = inputs'.devshell.legacyPackages.mkShell {
      name = "emacs-config-dev";

      devshell.startup.pre-commit-install.text = config.pre-commit.installationScript;

      packages = [pkgs.gdb pkgs.git pkgs.nixVersions.stable];

      commands = [
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
          config.packages.emacsEnv
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
          config.packages.emacsEnv
        ];
        text = ''
          XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
          export XDG_CONFIG_HOME
          mkdir -p "$XDG_CONFIG_HOME/emacs"
          lndir -silent ${config.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
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
          config.packages.emacsEnv
        ];
        text = ''
          XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
          export XDG_CONFIG_HOME
          mkdir -p "$XDG_CONFIG_HOME/emacs"
          lndir -silent ${config.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
          ln -s "$HOME/.config/fontconfig" "$XDG_CONFIG_HOME/."
          emacs -fs --load ${../../screenshots.el} --eval '(kill-emacs)'
        '';
      };
    };
  };
}
