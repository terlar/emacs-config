{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [$0];
}
