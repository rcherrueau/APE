{ pkgs ? (import <nixpkgs> {})
}:

# pkgsi686 -- 32bit env
pkgs.pkgsi686Linux.mkShell {
  buildInputs = [ pkgs.racket pkgs.binutils pkgs.file ];
}
