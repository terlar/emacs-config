{
  lib,
  org-babel-lib,
  twist-lib,
  rootPath,
  melpaSrc,
  pkgs,
  emacs,
}:

let
  initEl = lib.pipe (rootPath + "/init.org") [
    builtins.readFile
    (org-babel-lib.tangleOrgBabel { })
    (builtins.toFile "init.el")
  ];

  packageOverrides = _: prev: {
    elispPackages = prev.elispPackages.overrideScope (
      pkgs.callPackage ./packageOverrides.nix { inherit (prev) emacs; }
    );
  };
in
(twist-lib.makeEnv {
  inherit pkgs;
  emacsPackage = emacs;

  initFiles = [ initEl ];
  lockDir = rootPath + "/lock";

  registries = import ./registries.nix { inherit rootPath melpaSrc; };

  inputOverrides = import ./inputOverrides.nix { inherit rootPath lib; };

  initialLibraries = [
    "cl-lib"
    "jsonrpc"
    "let-alist"
    "map"
    "org"
    "seq"
    "transient"
  ];

  localPackages = [
    "pairable"
    "readable"
    "readable-mono-theme"
    "readable-typo-theme"
  ];
}).overrideScope
  packageOverrides
