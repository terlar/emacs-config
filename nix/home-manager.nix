{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  cfg = config.custom.emacsConfig;

  emacsEdit = if cfg.enableServer then "emacsclient" else "emacs";
  emacsDesktop = if cfg.enableServer then "emacsclient.desktop" else "emacs.desktop";
  emacsMailDesktop = if cfg.enableServer then "emacsclient-mail.desktop" else "emacs-mail.desktop";
in
{
  options.custom.emacsConfig = {
    enable = mkEnableOption "custom emacs configuration";

    package = mkOption {
      type = types.package;
      default = pkgs.emacs-env;
      defaultText = literalExample "pkgs.emacs-env";
      description = "The default Emacs derivation to use.";
    };

    configPackage = mkOption {
      type = types.package;
      default = pkgs.emacs-config;
      defaultText = literalExample "pkgs.emacs-config";
      description = "The default Emacs config derivation to use.";
    };

    enableUserDirectory = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable user Emacs directory files.";
    };

    enableGitDiff = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable ediff as default git diff tool.";
    };

    enableServer = mkOption {
      default = pkgs.stdenv.isLinux;
      type = types.bool;
      description = "Whether to enable user Emacs server.";
    };

    defaultEditor = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to use Emacs as default editor.";
    };

    defaultEmailApplication = mkOption {
      default = false;
      type = types.bool;
      description = "Whether to use Emacs as default e-mail application.";
    };

    defaultPdfApplication = mkOption {
      default = false;
      type = types.bool;
      description = "Whether to use Emacs as default PDF application.";
    };

    gnus = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Gnus config file.";
    };

    erc = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "ERC config file.";
    };

    private = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Private config file.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.emacs = {
        enable = cfg.enableServer;
        inherit (cfg) package;
        socketActivation.enable = true;
        extraOptions = [ "--no-desktop" ];
      };

      programs.git.extraConfig = {
        difftool.ediff.cmd = ''
          ${emacsEdit} --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
        '';

        mergetool.ediff.cmd = ''
          ${emacsEdit} --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" "'$BASE'" nil "'$MERGED'")'
        '';
      };

      home.packages = [ cfg.package ] ++ optionals cfg.enableUserDirectory cfg.configPackage.buildInputs;
    }
    (mkIf cfg.enableUserDirectory {
      xdg.configFile.emacs = {
        source = cfg.configPackage;
        recursive = true;
      };
    })
    (mkIf cfg.defaultEditor { home.sessionVariables.EDITOR = emacsEdit; })
    (mkIf cfg.enableGitDiff { programs.git.extraConfig.diff.tool = "ediff"; })
    (mkIf cfg.defaultEditor {
      programs.qutebrowser.settings.editor.command = [
        emacsEdit
        "{}"
      ];
    })
    (mkIf cfg.defaultEmailApplication {
      xdg.mimeApps.defaultApplications."x-scheme-handler/mailto" = emacsMailDesktop;
    })
    (mkIf cfg.defaultPdfApplication {
      xdg.mimeApps.defaultApplications."application/pdf" = emacsDesktop;
    })
    (mkIf (cfg.gnus != null) { home.file.".gnus.el".source = cfg.gnus; })
    (mkIf (cfg.erc != null) { xdg.configFile."emacs/.ercrc.el".source = cfg.erc; })
    (mkIf (cfg.private != null) { xdg.configFile."emacs/private/private.el".source = cfg.private; })
  ]);
}
