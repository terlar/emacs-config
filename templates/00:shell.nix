with (import <nixpkgs> {});

mkShell {
  buildInputs = [$0];
}
