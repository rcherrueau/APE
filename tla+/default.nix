# Note: run it with `nix-shell -I nixpkgs=./nixpkgs` to get the
# tlaplus-tools derivation.
# Find spacemacs layer at
# https://github.com/rcherrueau/.dotfiles/tree/c74de7cd68e8264e431873076185b6b12517d006/spacemacs/.emacs.d/private/tla%2B
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
