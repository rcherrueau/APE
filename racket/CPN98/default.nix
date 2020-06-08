{ pkgs ? (import <nixpkgs> {})
, racket2nix ? (import (pkgs.fetchFromGitHub {
    owner = "fractalide"; repo = "racket2nix";
    rev = "15d2bf5708ca01ec4d9035c4369ce1db04b9bddd";
    sha256 = "0plwvq4nwgrsz541azw3xw593b2r1zj9vmjzxrar8vi2vpdmi5ll";
  }) {})
, temci ? (import (pkgs.fetchFromGitHub {
    owner = "parttimenerd"; repo = "temci";
    rev = "a741e6eb9c427a0a21a7d04ed430561d51dced2d";
    sha256 = "0kr9cb0630vdxrpcp5vja43rln6s7sqxqanw75n1726wz5plvqmn";
  }) {})
}:

# let
#   dynext = racket2nix.buildRacketPackage "dynext-lib";
#   typed-map = racket2nix.buildRacketPackage "typed-map-lib";
# in
pkgs.mkShell {
  buildInputs = [
    # pkgs.racket
    # temci  # mesure performances
  ];

  shellHook = ''
    export PLTSTDOUT="debug@sclang"
  '';
}
