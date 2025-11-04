{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) types;
  cfg = config.custom.emacsConfig;

  emacsEdit = if cfg.enableServer then "emacsclient" else "emacs";
  emacsDesktop = if cfg.enableServer then "emacsclient.desktop" else "emacs.desktop";
  emacsMailDesktop = if cfg.enableServer then "emacsclient-mail.desktop" else "emacs-mail.desktop";
in
{
  options.custom.emacsConfig = {
    enable = lib.mkEnableOption "custom emacs configuration";

    package = lib.mkOption {
      type = types.package;
      description = "The default Emacs derivation to use.";
    };

    configPackage = lib.mkOption {
      type = types.package;
      description = "The default Emacs config derivation to use.";
    };

    enableUserDirectory = lib.mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable user Emacs directory files.";
    };

    enableGitDiff = lib.mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable ediff as default git diff tool.";
    };

    enableServer = lib.mkOption {
      default = pkgs.stdenv.isLinux;
      type = types.bool;
      description = "Whether to enable user Emacs server.";
    };

    defaultEditor = lib.mkOption {
      default = true;
      type = types.bool;
      description = "Whether to use Emacs as default editor.";
    };

    defaultEmailApplication = lib.mkOption {
      default = false;
      type = types.bool;
      description = "Whether to use Emacs as default e-mail application.";
    };

    defaultPdfApplication = lib.mkOption {
      default = false;
      type = types.bool;
      description = "Whether to use Emacs as default PDF application.";
    };

    gnus = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Gnus config file.";
    };

    erc = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "ERC config file.";
    };

    private = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Private config file.";
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        services.emacs = {
          inherit (cfg) package;
          enable = lib.mkDefault cfg.enableServer;
          client.enable = lib.mkDefault true;
          socketActivation.enable = lib.mkDefault true;
          extraOptions = [ "--no-desktop" ];
        };

        programs.git.settings = {
          difftool.ediff.cmd = ''
            ${emacsEdit} --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
          '';

          mergetool.ediff.cmd = ''
            ${emacsEdit} --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" "'$BASE'" nil "'$MERGED'")'
          '';
        };

        home.packages = [
          cfg.package
        ]
        ++ lib.optionals cfg.enableUserDirectory cfg.configPackage.runtimeInputs;
      }
      (lib.mkIf cfg.enableUserDirectory {
        xdg.configFile.emacs = {
          source = cfg.configPackage;
          recursive = true;
        };
      })
      (lib.mkIf cfg.defaultEditor {
        home.sessionVariables.EDITOR = emacsEdit;
        programs.qutebrowser.settings.editor.command = [
          emacsEdit
          "{}"
        ];
      })
      (lib.mkIf cfg.enableGitDiff { programs.git.settings.diff.tool = "ediff"; })
      (lib.mkIf cfg.defaultEmailApplication {
        xdg.mimeApps.defaultApplications."x-scheme-handler/mailto" = emacsMailDesktop;
      })
      (lib.mkIf cfg.defaultPdfApplication {
        xdg.mimeApps.defaultApplications."application/pdf" = emacsDesktop;
      })
      (lib.mkIf (cfg.gnus != null) { home.file.".gnus.el".source = cfg.gnus; })
      (lib.mkIf (cfg.erc != null) { xdg.configFile."emacs/.ercrc.el".source = cfg.erc; })
      (lib.mkIf (cfg.private != null) { xdg.configFile."emacs/private/private.el".source = cfg.private; })
    ]
  );
}
