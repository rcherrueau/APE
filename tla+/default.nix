# Note: run it with `nix-shell -I nixpkgs=./nixpkgs` to get the
# tlaplus-tools derivation.
with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "tla+-in-practice";
  buildInputs = [ jdk tlaplus-tools ];

  shellHook = ''
    # Nix handles that for me :)
    # export CLASSPATH=$CLASSPATH:"${tlaplus-tools}/share/java/tla2tools,jar"
    emacs &
  '';
}
