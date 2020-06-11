{ config, lib, pkgs, ... }:

with builtins;
with lib;

let
  cfg = config.custom.emacsConfig;

  emacsEdit = if cfg.enableUtils then
    "emacseditor"
  else
    (if enableServer then "emacsclient" else "emacs");

  extraPackages = import ./packages.nix;
  overrides = pkgs.callPackage ./overrides.nix { };

  mkEmacsConfigFiles = path:
    foldl' (acc: file: acc // { "emacs/${file}".source = "${path}/${file}"; })
    { } (attrNames (readDir path));
in {
  options.custom.emacsConfig = {
    enable = mkEnableOption "custom emacs configuration";

    package = mkOption {
      type = types.package;
      default = pkgs.emacsGit;
      defaultText = literalExample "pkgs.emacsGit";
      description = "The default Emacs derivation to use.";
    };

    enableOverrides = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable Emacs package overrides.";
    };

    enablePackages = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable extra Emacs packages.";
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
      default = true;
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

    enableUtils = mkOption {
      default = true;
      type = types.bool;
      description = "Whether to enable Emacs utils.";
    };
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = cfg.package;
      extraPackages = mkIf cfg.enablePackages extraPackages;
      overrides = mkIf cfg.enableOverrides overrides;
    };

    services.emacs.enable = cfg.enableServer;

    xdg.configFile =
      mkIf cfg.enableUserDirectory (mkEmacsConfigFiles pkgs.emacsConfig);

    home.sessionVariables = mkIf cfg.defaultEditor { EDITOR = emacsEdit; };

    programs.git.extraConfig = {
      diff.tool = mkIf cfg.enableGitDiff "ediff";

      "difftool \"ediff\"".cmd = ''
        ${emacsEdit} --eval '(ediff-files "'$LOCAL'" "'$REMOTE'")'
      '';

      "mergetool \"ediff\"".cmd = ''
        ${emacsEdit} --eval '(ediff-merge-files-with-ancestor "'$LOCAL'" "'$REMOTE'" "'$BASE'" nil "'$MERGED'")'
      '';
    };

    programs.qutebrowser.settings.editor.command =
      mkIf cfg.defaultEditor [ emacsEdit "{}" ];

    home.packages = [ ]
      ++ optionals cfg.enableUserDirectory pkgs.emacsConfig.buildInputs
      ++ optional cfg.enableUtils pkgs.emacsUtils;

    xdg.mimeApps.defaultApplications = {
      "x-scheme-handler/mailto" =
        mkIf (cfg.enableUtils && cfg.defaultEmailApplication)
        "emacsmail.desktop";
      "application/pdf" = mkIf (cfg.enableUtils && cfg.defaultPdfApplication)
        "emacseditor.desktop";
    };
  };
}
