{self, ...}: {
  perSystem = {
    pkgs,
    self',
    inputs',
    ...
  }: {
    formatter = pkgs.alejandra;

    devShells.default = inputs'.devshell.legacyPackages.mkShell {
      name = "emacs-config-dev";

      packages = [pkgs.gdb pkgs.git pkgs.nixVersions.stable];

      commands = [
        {
          package = self'.packages.devEmacsConfig;
          help = "launch bundled Emacs with configuration from source";
          category = "emacs";
        }
        {
          package = self'.packages.reloadEmacsConfig;
          help = "reload and launch Emacs service";
          category = "emacs";
        }
        {
          package = self'.packages.testEmacsConfig;
          help = "launch bundled Emacs with fixed configuration";
          category = "emacs";
        }
        {
          package = self'.packages.updateCaches;
          help = "update Nix caches";
          category = "nix";
        }
        {
          package = self'.packages.updateScreenshots;
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
          self'.packages.emacsEnv
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
          self'.packages.emacsEnv
        ];
        text = ''
          XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
          export XDG_CONFIG_HOME
          mkdir -p "$XDG_CONFIG_HOME/emacs"
          lndir -silent ${self'.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
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
          self'.packages.emacsEnv
        ];
        text = ''
          XDG_CONFIG_HOME="$(mktemp -td xdg-config.XXXXXXXXXX)"
          export XDG_CONFIG_HOME
          mkdir -p "$XDG_CONFIG_HOME/emacs"
          lndir -silent ${self'.packages.emacsConfig} "$XDG_CONFIG_HOME/emacs"
          ln -s "$HOME/.config/fontconfig" "$XDG_CONFIG_HOME/."
          emacs -fs --load ${../../screenshots.el} --eval '(kill-emacs)'
        '';
      };
    };
  };
}
