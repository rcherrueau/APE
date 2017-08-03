with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "pgdump2cockroach";
  buildInputs = [ rustStable.rustc rustStable.cargo ];
  shellHook = ''
    # eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}
