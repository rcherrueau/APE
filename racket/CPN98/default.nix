{ pkgs ? (import <nixpkgs> {})
, racket2nix ? (import (pkgs.fetchFromGitHub {
    owner = "fractalide"; repo = "racket2nix";
    rev = "15d2bf5708ca01ec4d9035c4369ce1db04b9bddd";
    sha256 = "0plwvq4nwgrsz541azw3xw593b2r1zj9vmjzxrar8vi2vpdmi5ll";
  }) {})
, temci ? (import (pkgs.fetchFromGitHub {
    owner = "parttimenerd"; repo = "temci";
    rev = "64c2f43f68b9e0aae8f47730ea7de9b5e762d1a9";
    sha256 = "1b4l8zik24w3ifcma4jz5wvndg6886r4g1vlv9rrwxpcqll3wzb6";
  }) {})
}:

# let
#   dynext = racket2nix.buildRacketPackage "dynext-lib";
#   typed-map = racket2nix.buildRacketPackage "typed-map-lib";
# in
pkgs.mkShell {
  buildInputs = [
    # pkgs.racket
    temci  # mesure performances
  ];

  shellHook = ''
  '';

  # Set environment variables
  PLTSTDOUT = "debug@sclang";
}
