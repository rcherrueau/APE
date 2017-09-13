with import <nixpkgs> {};

let racket-with-doc = racket.override { disableDocs = false; };
in stdenv.mkDerivation {
  name = "racket-env";
  buildInputs = [
    racket-with-doc
  ];
  shellHook = ''
    ${racket-with-doc}/bin/raco pkg install beautiful-racket
    ${racket-with-doc}/bin/raco pkg install megaparsack
    emacs &
  '';

}
