{ pkgs ? (import <nixpkgs> {})
}:

pkgs.mkShell {
  buildInputs = [ pkgs.racket ];
}
