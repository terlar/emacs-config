{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

mkShell {
  buildInputs = [$0];
}
