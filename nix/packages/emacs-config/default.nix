{
  lib,
  twist-lib,
  pkgs,
  emacs,
  emacs-env,
  emacsPackages,
  rootPath,
}:

pkgs.callPackage ./build-config.nix {
  inherit rootPath;

  buildElispPackage = (twist-lib.buildElispPackage pkgs).override {
    inherit emacs;
  };

  elispInputs = lib.pipe emacs-env.elispPackages [
    builtins.attrValues
    (builtins.filter lib.isDerivation)
  ];

  treesit-grammars = emacsPackages.treesit-grammars.with-grammars (ps: [
    ps.tree-sitter-dockerfile
    ps.tree-sitter-elixir
    ps.tree-sitter-go
    ps.tree-sitter-gomod
    ps.tree-sitter-heex
    ps.tree-sitter-java
    ps.tree-sitter-javascript
    ps.tree-sitter-json
    ps.tree-sitter-lua
    ps.tree-sitter-nix
    ps.tree-sitter-python
    ps.tree-sitter-ruby
    ps.tree-sitter-rust
    ps.tree-sitter-typescript
    ps.tree-sitter-yaml
  ]);
}
