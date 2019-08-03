{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};

  projectDrvEnv = (import ./default.nix { inherit nixpkgs compiler; }).env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [
      haskellPackages.stack
      haskellPackages.hlint
      haskellPackages.hoogle
      haskellPackages.structured-haskell-mode
      haskellPackages.stylish-haskell
      all-hies.versions.${compiler}
    ];
  });
in projectDrvEnv
